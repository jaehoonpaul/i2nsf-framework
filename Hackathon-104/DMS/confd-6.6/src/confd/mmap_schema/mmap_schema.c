#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <netdb.h>
#include <string.h>
#include <stdlib.h>
#include <inttypes.h>
#include <stdio.h>
#include <unistd.h>
#include <fcntl.h>
#include <sys/utsname.h>
#include <sys/param.h>

#include <confd_lib.h>

#define MB (1024 * 1024)
#define SCHEMA_FILE "schemas"
#define DISABLED 0
#define ENABLED 1

#define OK(E) do {                                                      \
        int _ret = (E);                                                 \
        if (_ret != CONFD_OK) {                                         \
            confd_fatal(                                                \
                "%s returned %d, confd_errno=%d, confd_lasterr()='%s'\n", \
                #E, _ret, confd_errno, confd_lasterr());                \
        }                                                               \
    } while (0)

int loop(int);

typedef unsigned char byte;


int read_exact(byte *buf, int len)
{
    int i, got=0;

    do {
        if ((i = read(3, buf+got, len-got)) <= 0)
            return(i);
        got += i;
    } while (got<len);

    return(len);
}

int write_exact(byte *buf, int len)
{
    int i, wrote = 0;

    do {
        if ((i = write(4, buf+wrote, len-wrote)) <= 0)
            return (i);
        wrote += i;
    } while (wrote<len);

    return (len);
}

int tbh_read(byte *buf, int maxlen)
{
    int len;

    if (read_exact(buf, 2) != 2)
        return(-1);
    len = (buf[0] << 8) | buf[1];

    if (len < maxlen)
        return read_exact(buf, len);
    else
        return -1;
}

int tbh_write(byte *buf, int len)
{
    byte li;

    li = (len >> 8) & 0xff;
    write_exact(&li, 1);

    li = len & 0xff;
    write_exact(&li, 1);

    return write_exact(buf, len);
}

static void *get_shm_addr(size_t offset)
{
    size_t pagesize;
    char *addr;

    pagesize = (size_t)sysconf(_SC_PAGESIZE);
    assert(pagesize != (size_t)-1);
    addr = malloc(1);
    free(addr);
    addr += offset;
    /* return pagesize-aligned address */
    return addr - ((uintptr_t)addr % pagesize);
}

#define PORT_SIZE 16

int main(int argc, char **argv)
{
    char confd_port[PORT_SIZE];
    char ncs_port[PORT_SIZE];
    char port[PORT_SIZE];
    char ipstr[64];
    struct addrinfo hints;
    struct addrinfo *addr = NULL;
    int debuglevel = CONFD_SILENT;
    int c;
    char *p, *dname;
    int i;
    struct utsname unm;
    void *shm_addr;
    size_t shm_size;
    int shm_flags;
    char file_path[MAXPATHLEN];

    /* Disable buffering */
    setbuf(stdout, NULL);

    snprintf(confd_port, sizeof(confd_port), "%d", CONFD_PORT);
    snprintf(ncs_port, sizeof(ncs_port), "%d", NCS_PORT);
    memset(&hints, 0, sizeof(hints));
    hints.ai_family = PF_UNSPEC;
    hints.ai_socktype = SOCK_STREAM;
    memset(file_path, 0, sizeof(file_path));


    // defaults
    snprintf(ipstr, sizeof(ipstr), "%s", "127.0.0.1");
    snprintf(port, sizeof(port), "%s", confd_port);

    while ((c = getopt(argc, argv, "dtpc:f:I:P:E:A:")) != -1) {
        switch (c) {
        case 'd':
            debuglevel = CONFD_DEBUG;
            break;
        case 't':
            debuglevel = CONFD_TRACE;
            break;
        case 'p':
            debuglevel = CONFD_PROTO_TRACE;
            break;
        case 'c':
            if ((p = strchr(optarg, '/')) != NULL)
                *p++ = '\0';
            else
                //p = confd_port;
                p = ncs_port;
            if (getaddrinfo(optarg, p, &hints, &addr) != 0) {
                //if (p != confd_port) {
                if (p != ncs_port) {
                    *--p = '/';
                    p = "/port";
                } else {
                    p = "";
                }
                fprintf(stderr, "%s: Invalid address%s: %s\n",
                        argv[0], p, optarg);
                exit(1);
            }
            break;

        case 'f':
            snprintf(file_path, sizeof(file_path), "%s", optarg);
            break;

        case 'I':
            snprintf(ipstr, sizeof(ipstr), "%s", optarg);
            break;

        case 'P':
            snprintf(port, sizeof(port), "%s", optarg);
            break;

            // -A and -E is not implemented (external IPC)
        case 'A':
            // this is the address for external IPC
            fprintf(stderr,
                    "external IPC not implemented: %s\n",
                    argv[0]);
            exit(1);
        case 'E':
            // this is the path to the shared object for external IPC
            fprintf(stderr,
                    "external IPC not implemented: %s\n",
                    argv[0]);
            exit(1);

        default:
            fprintf(stderr,
                    "Usage: %s [-dtpl] [-c address[/port]]\n",
                    argv[0]);
            exit(1);
        }
    }

    if ((dname = strrchr(argv[0], '/')) != NULL)
        dname++;
    else
        dname = argv[0];
    confd_init(dname, stderr, debuglevel);

    if (addr == NULL &&
        (i = getaddrinfo(ipstr, port, &hints, &addr)) != 0)
        /* "Can't happen" */
        confd_fatal("%s: Failed to get address for ConfD: %s\n",
                    argv[0], gai_strerror(i));

    assert(uname(&unm) >= 0);
    if (strcmp(unm.sysname, "Linux") == 0 ||
        (strcmp(unm.sysname, "Darwin") == 0 &&
         strcmp(unm.machine, "i386") ==0)) {
        /* Linux and Darwin/i386 seem happy with heap top + 256 MB */
        shm_addr = get_shm_addr(256 * MB);
        shm_size = 0;
        shm_flags = 0;
        /* printf("Mapping at address 0x%" PRIxPTR "\n", (uintptr_t)shm_addr);*/
    } else if (strcmp(unm.sysname, "FreeBSD") == 0) {
        /* FreeBSD-6.0/i386 prefers heap top + 1 GB - OK for 8.2/amd64 too */
        shm_addr = get_shm_addr(1024 * MB);
        shm_size = 0;
        shm_flags = 0;
        /* printf("Mapping at address 0x%" PRIxPTR "\n", (uintptr_t)shm_addr);*/
    } else if (strcmp(unm.sysname, "SunOS") == 0) {
        /* Solaris requires fixed size, ignores given addr */
        shm_addr = NULL;
        shm_size = 2 * MB;
        shm_flags = CONFD_MMAP_SCHEMAS_KEEP_SIZE;
        /* printf("Mapping size 0x%" PRIxPTR " with KEEP_SIZE\n",
               (uintptr_t)shm_size); */
    } else if (strcmp(unm.sysname, "Darwin") == 0 &&
               strcmp(unm.machine, "x86_64") ==0) {
        shm_addr = get_shm_addr(256 * MB);
        shm_size = 0;
        shm_flags = 0;
        /* printf("Mapping at address 0x%" PRIxPTR
               " with CONFD_MMAP_SCHEMAS_FIXED_ADDR\n", (uintptr_t)shm_addr); */
    } else {
        /* simple method - NULL addr and hefty size - does it work anywhere? */
        shm_addr = NULL;
        shm_size = 32 * MB;
        shm_flags = 0;
        /* printf("Mapping size 0x%" PRIxPTR "\n", (uintptr_t)shm_size); */
    }

    if (strlen(file_path) > 0) {
        /* disable as can crash at times under darwin */
        if (strcmp(unm.sysname, "Darwin") == 0) {
            loop(DISABLED);
        } else {
            OK(confd_mmap_schemas_setup(shm_addr, shm_size,
                                        file_path, shm_flags));
            OK(confd_load_schemas(addr->ai_addr, addr->ai_addrlen));
            loop(ENABLED);
        }
        freeaddrinfo(addr);
    } else {
        byte emsg[] = "mmap_schema: ERROR no file-path specified\n";
        tbh_write(emsg, sizeof(emsg) - 1);
    }

    return 0;
}


int loop(int state) {
    byte buf[1024];
    int i=0;
    byte emsg[] = "Exiting!";
    byte not_supported[] = "not_supported";

    while((i=tbh_read(buf, sizeof(buf))) > 0) {
        if (state == ENABLED) {
            buf[i] = 0;
            tbh_write(buf, i);
        } else {
            tbh_write(not_supported, sizeof(not_supported) - 1);
        }
    }
    tbh_write(emsg, sizeof(emsg) - 1);

    return 0;
}
