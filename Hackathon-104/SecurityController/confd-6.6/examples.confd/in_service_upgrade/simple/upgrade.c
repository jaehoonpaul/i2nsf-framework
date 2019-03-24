
#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <string.h>
#include <unistd.h>
#include <errno.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <netdb.h>
#include <signal.h>

#include <confd_lib.h>
#include <confd_maapi.h>
#include <confd_cdb.h>

#define PKG_DIR      "./pkg"
#define VSN_LINK     PKG_DIR "/current"
#define VSN_LINK_NEW VSN_LINK ".new"

static struct addrinfo *addr;
static int maapisock;
static char *usid_env = NULL;
static pid_t notifier = -1;
static char *phase;
static char old_version[BUFSIZ];

static void progress(char *msg)
{
    if (usid_env != NULL)
        maapi_prio_message(maapisock, usid_env, msg);
    else
        printf("%s", msg);
}

static void OK(int ret)
{
    char buf[BUFSIZ];

    if (ret == CONFD_OK)
        return;
    snprintf(buf, sizeof(buf), "%s - %s\n%s FAILED\n",
             confd_strerror(confd_errno), confd_lasterr(), phase);
    progress(buf);
    if (notifier != -1)
        kill(notifier, SIGTERM);
    maapi_prio_message(maapisock, "all",
                       ">>> System upgrade has been cancelled.\n");
    exit(1);
}

static int get_maapisock(struct addrinfo *addr)
{
    int sock;

    sock = socket(addr->ai_family, addr->ai_socktype, addr->ai_protocol);
    if (sock < 0)
        return -1;
    if (maapi_connect(sock, addr->ai_addr, addr->ai_addrlen) != CONFD_OK) {
        close(sock);
        return -1;
    }
    return sock;
}

static int set_usess(int sock)
{
    char *user = "admin";
    const char *groups[] = {"admin"};
    char *context = "system";
    struct confd_ip ip;

    /* must use usid from CLI to be allowed to run across upgrade */
    if ((usid_env = getenv("CONFD_MAAPI_USID")) != NULL) {
        return maapi_set_user_session(sock, atoi(usid_env));
    } else {
        ip.af = AF_INET;
        inet_pton(AF_INET, "127.0.0.1", &ip.ip.v4);
        return maapi_start_user_session(sock, user, context, groups, 1,
                                        &ip, CONFD_PROTO_TCP);
    }
}

/* change loadpath atomically */
static void relink(char *version)
{
    unlink(VSN_LINK_NEW);
    /* These should never fail */
    if (symlink(version, VSN_LINK_NEW) != 0)
        confd_fatal("Failed to create symlink for new version: %s\n",
                    strerror(errno));
    if (rename(VSN_LINK_NEW, VSN_LINK) != 0)
        confd_fatal("Failed to rename symlink for new version: %s\n",
                    strerror(errno));
}

static pid_t run_notifier(int timeout, int force)
{
    pid_t pid;
    int sock;
    time_t now, then;
    int remain;
    char msg[BUFSIZ];

    /* run in separate process */
    if ((pid = fork()) != 0)
        return pid;

    time(&now);
    then = now + timeout;
    if ((sock = get_maapisock(addr)) < 0)
        exit(1);
    snprintf(msg, sizeof(msg),
             "\n>>> System upgrade is starting.\n"
             ">>> Sessions in configure mode %s operational mode.\n"
             ">>> No configuration changes can be performed until "
             "upgrade has completed.\n",
             force ? "will be forced into" : "must exit to");
    maapi_prio_message(sock, "all", msg);
    sleep(10);

    time(&now);
    while ((remain = then - now) > 0) {
        if (force) {
            snprintf(msg, sizeof(msg),
                     ">>> System upgrade is pending.\n"
                     ">>> Sessions in configure mode will be forced\n"
                     ">>> into operational mode in %d seconds.\n",
                     remain);
        } else {
            snprintf(msg, sizeof(msg),
                     ">>> System upgrade is pending.\n"
                     ">>> Sessions must exit from configure mode now.\n");
        }
        maapi_prio_message(sock, "all", msg);
        sleep(10);
        time(&now);
    }
    exit(0);
}

static void usage(char *argv0)
{
    fprintf(stderr, "Usage: %s [-f] -t 'timeout' -v 'version'\n", argv0);
    exit(1);
}

static void update_version_in_cdb_oper(struct addrinfo *addr, char *version)
{
    int sock;

    sock = socket(addr->ai_family, addr->ai_socktype, addr->ai_protocol);
    if (sock < 0)
        return;
    if (cdb_connect(sock, CDB_DATA_SOCKET, addr->ai_addr, addr->ai_addrlen)
        != CONFD_OK) {
        close(sock);
        return;
    }
    if (cdb_start_session2(sock, CDB_OPERATIONAL, 0) != CONFD_OK) {
        cdb_close(sock);
        return;
    }
    cdb_set_elem2(sock, version, "/simple:stats/version");
    cdb_close(sock);
}

int main(int argc, char **argv)
{
    char confd_port[16];
    struct addrinfo hints;
    int debuglevel = CONFD_SILENT;
    int c, i, n;
    char *version = NULL;
    int timeout = -1;
    int force = 0;
    char *dname;
    struct stat sb;
    char buf[BUFSIZ];
    const char *load_dir[] = {NULL, CONFD_FXS_DIR};
    int ndirs = sizeof(load_dir) / sizeof(load_dir[0]);

    while ((c = getopt(argc, argv, "v:t:f")) != -1) {
        switch (c) {
        case 'v':
            version = optarg;
            break;
        case 't':
            timeout = atoi(optarg);
            break;
        case 'f':
            force = 1;
            break;
        default:
            usage(argv[0]);
        }
    }
    if (timeout < 0 || version == NULL)
        usage(argv[0]);

    /* sanity checks */
    snprintf(buf, sizeof(buf), PKG_DIR "/%s", version);
    if (stat(buf, &sb) != 0 || !S_ISDIR(sb.st_mode)) {
        printf("Version %s does not exist!\n", version);
        exit(1);
    }
    if ((n = readlink(VSN_LINK, old_version, sizeof(old_version)-1)) > 0) {
        old_version[n] = '\0';
        if (strcmp(old_version, version) == 0) {
            printf("System is already running version %s!\n", version);
            exit(1);
        }
    }

    /* set up for connection to ConfD */
    snprintf(confd_port, sizeof(confd_port), "%d", CONFD_PORT);
    memset(&hints, 0, sizeof(hints));
    hints.ai_family = PF_UNSPEC;
    hints.ai_socktype = SOCK_STREAM;
    if ((i = getaddrinfo("127.0.0.1", confd_port, &hints, &addr)) != 0)
        /* "Can't happen" */
        confd_fatal("%s: Failed to get address for ConfD: %s\n",
                    argv[0], gai_strerror(i));
    if ((dname = strrchr(argv[0], '/')) != NULL)
        dname++;
    else
        dname = argv[0];

    /* Init library and connect */
    confd_init(dname, stderr, debuglevel);
    if ((maapisock = get_maapisock(addr)) < 0 ||
        maapi_load_schemas(maapisock) != CONFD_OK ||
        set_usess(maapisock) != CONFD_OK)
        confd_fatal("Failed to connect maapi socket to ConfD\n");


    /* Do the upgrade */

    progress("Initializing upgrade...\n");
    phase = "Init";
    /* run notifier in separate process
       - maapi_init_upgrade() blocks */
    notifier = run_notifier(timeout, force);
    OK(maapi_init_upgrade(maapisock, timeout,
                          force ? MAAPI_UPGRADE_KILL_ON_TIMEOUT : 0));
    if (notifier != -1)
        kill(notifier, SIGTERM);
    notifier = -1;
    progress("Init OK\n");
    maapi_prio_message(maapisock, "all",
                       "\n>>> System upgrade in progress...\n");

    progress("Performing upgrade...\n");
    phase = "Perform";
    /* set up new loadpath directory */
    snprintf(buf, sizeof(buf), PKG_DIR "/%s", version);
    load_dir[0] = &buf[0];
    OK(maapi_perform_upgrade(maapisock, &load_dir[0], ndirs));
    progress("Perform OK\n");

    progress("Committing upgrade...\n");
    phase = "Commit";
    OK(maapi_commit_upgrade(maapisock));
    relink(version);
    progress("Commit OK\n");

    maapi_prio_message(maapisock, "all",
                       ">>> System upgrade has completed successfully.\n");

    update_version_in_cdb_oper(addr, version);

    return 0;
}
