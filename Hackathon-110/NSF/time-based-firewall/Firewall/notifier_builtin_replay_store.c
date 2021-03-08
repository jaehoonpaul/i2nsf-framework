#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <netdb.h>
#include <sys/poll.h>
#include <time.h>
#include <sys/time.h>
#include <stdbool.h>
#include <stdarg.h>
#include <json-c/json.h>

#include <confd_lib.h>
#include <confd_dp.h>

#include "ietf-i2nsf-nsf-monitoring.h"

#define OK(val) (assert((val) == CONFD_OK))

static int ctlsock, workersock;
static struct confd_daemon_ctx *dctx;
static struct confd_notification_ctx *live_ctx;

static int get_ctlsock(struct addrinfo *addr)
{
    int sock;

    if ((sock =
         socket(addr->ai_family, addr->ai_socktype, addr->ai_protocol)) < 0)
        return -1;
    if (confd_connect(dctx, sock, CONTROL_SOCKET,
                      addr->ai_addr, addr->ai_addrlen) != CONFD_OK) {
        close(sock);
        return -1;
    }
    return sock;
}

static int get_workersock(struct addrinfo *addr)
{
    int sock;

    if ((sock =
         socket(addr->ai_family, addr->ai_socktype, addr->ai_protocol)) < 0)
        return -1;
    if (confd_connect(dctx, sock, WORKER_SOCKET,
                      addr->ai_addr, addr->ai_addrlen) != CONFD_OK) {
        close(sock);
        return -1;
    }
    return sock;
}

static void getdatetime(struct confd_datetime *datetime)
{
    struct tm tm;
    struct timeval tv;

    gettimeofday(&tv, NULL);
    gmtime_r(&tv.tv_sec, &tm);

    memset(datetime, 0, sizeof(*datetime));
    datetime->year = 1900 + tm.tm_year;
    datetime->month = tm.tm_mon + 1;
    datetime->day = tm.tm_mday;
    datetime->sec = tm.tm_sec;
    datetime->micro = tv.tv_usec;
    datetime->timezone = 0;
    datetime->timezone_minutes = 0;
    datetime->hour = tm.tm_hour;
    datetime->min = tm.tm_min;
}

static void send_notification(confd_tag_value_t *vals, int nvals)
{
    struct confd_datetime eventTime;

    getdatetime(&eventTime);
    OK(confd_notification_send(live_ctx, &eventTime, vals, nvals));
}

static void send_notif_monitoring(int alarm_category, int usage, int threshold,  int acquisition_method, int emission_type , char *nsf_name, int severity, char *message)
{
    confd_tag_value_t vals[10];
    int i = 0;

    struct confd_identityref idref_alarm, idref_acqui, idref_emission;
    idref_alarm.id = nsfmi_MEM_USAGE_ALARM;
    idref_alarm.ns = nsfmi__ns;

    idref_acqui.id = nsfmi_subscription;
    idref_acqui.ns = nsfmi__ns;

    idref_emission.id = nsfmi_on_change;
    idref_emission.ns = nsfmi__ns;
    CONFD_SET_TAG_XMLBEGIN(&vals[i], nsfmi_i2nsf_system_detection_alarm,   nsfmi__ns);  i++;
    CONFD_SET_TAG_IDENTITYREF(&vals[i], nsfmi_alarm_category, idref_alarm);      i++;
    CONFD_SET_TAG_IDENTITYREF(&vals[i],  nsfmi_acquisition_method,  idref_acqui);  i++;
    CONFD_SET_TAG_IDENTITYREF(&vals[i],   nsfmi_emission_type,   idref_emission);  i++;
    CONFD_SET_TAG_UINT8(&vals[i],    nsfmi_usage,             usage);  i++;
    CONFD_SET_TAG_UINT8(&vals[i],    nsfmi_threshold,          threshold);  i++;
    CONFD_SET_TAG_STR(&vals[i],   nsfmi_message,    message); i++;
    CONFD_SET_TAG_STR(&vals[i],   nsfmi_nsf_name,   nsf_name);  i++;
    CONFD_SET_TAG_ENUM_VALUE(&vals[i],   nsfmi_severity,   severity);  i++;
    CONFD_SET_TAG_XMLEND(&vals[i],   nsfmi_i2nsf_system_detection_alarm,   nsfmi__ns);  i++;
    send_notification(vals, i);

}

static void send_notif_resources(char *status, int cpu_usage, int memory_usage,  int disk_usage, int disk_left , int in_traffic_speed, int out_traffic_speed, char *nsf_name)
{
    confd_tag_value_t vals[12];
    int i = 0;
    
    struct confd_identityref idref_acqui, idref_emission;
    idref_acqui.id = nsfmi_subscription;
    idref_acqui.ns = nsfmi__ns;

    idref_emission.id = nsfmi_on_change;
    idref_emission.ns = nsfmi__ns;
    
    CONFD_SET_TAG_XMLBEGIN(&vals[i], nsfmi_i2nsf_system_res_util_log,   nsfmi__ns);  i++;
    CONFD_SET_TAG_STR(&vals[i], nsfmi_system_status, status);      i++;
    CONFD_SET_TAG_UINT8(&vals[i],    nsfmi_cpu_usage,             cpu_usage);  i++;
    CONFD_SET_TAG_UINT8(&vals[i],    nsfmi_memory_usage,        memory_usage);  i++;
    CONFD_SET_TAG_UINT8(&vals[i],    nsfmi_disk_usage,          disk_usage);  i++;
    CONFD_SET_TAG_UINT8(&vals[i],    nsfmi_disk_left,          disk_left);  i++;
    CONFD_SET_TAG_UINT32(&vals[i],    nsfmi_in_traffic_speed,          in_traffic_speed);  i++;
    CONFD_SET_TAG_UINT32(&vals[i],    nsfmi_out_traffic_speed,          out_traffic_speed);  i++;
    CONFD_SET_TAG_IDENTITYREF(&vals[i],  nsfmi_acquisition_method,  idref_acqui);  i++;
    CONFD_SET_TAG_IDENTITYREF(&vals[i],   nsfmi_emission_type,   idref_emission);  i++;
    CONFD_SET_TAG_STR(&vals[i],   nsfmi_nsf_name,   nsf_name);  i++;
    CONFD_SET_TAG_XMLEND(&vals[i],   nsfmi_i2nsf_system_res_util_log,   nsfmi__ns);  i++;
    send_notification(vals, i);

}

static int datetime_le(struct confd_datetime *a, struct confd_datetime *b)
{
    unsigned int ax, bx;
    if (a->year < b->year) { return 1; }
    if (a->year>b->year) { return 0;}
    if (a->month<b->month) { return 1; }
    if (a->month>b->month) { return 0;}
    if (a->day<b->day) { return 1;}
    if (a->day>b->day) { return 0;}
    ax = a->hour - a->timezone;
    bx = b->hour - b->timezone;
    if (ax<bx) { return 1;}
    if (ax>bx) { return 0;}
    ax = a->min - a->timezone_minutes;
    bx = b->min - b->timezone_minutes;
    if (ax<bx) { return 1;}
    if (ax>bx) { return 0;}
    if (a->sec<b->sec) { return 1;}
    if (a->sec>b->sec) { return 0;}
    if (a->micro<b->micro) { return 1;}
    if (a->micro>b->micro) { return 0;}
    return 1;
}

char* concat(int count, ...)
{
    va_list ap;
    int i;

    // Find required length to store merged string
    int len = 1; // room for NULL
    va_start(ap, count);
    for(i=0 ; i<count ; i++)
        len += strlen(va_arg(ap, char*));
    va_end(ap);

    // Allocate memory to concat strings
    char *merged = calloc(sizeof(char),len);
    int null_pos = 0;

    // Actually concatenate strings
    va_start(ap, count);
    for(i=0 ; i<count ; i++)
    {
        char *s = va_arg(ap, char*);
        strcpy(merged+null_pos, s);
        null_pos += strlen(s);
    }
    va_end(ap);

    return merged;
}

struct json_object *getJson(char *key){
    FILE *fp;
    char buffer[1024];
    struct json_object *parsed_json;
    struct json_object *data;

    // GET CURRENT DATE
    time_t t = time(NULL);
    struct tm tm = *localtime(&t);
    char year[4], month[2], day[2];
    sprintf(year,"%d", tm.tm_year + 1900);
    sprintf(month,"%02d", tm.tm_mon + 1 );
    sprintf(day,"%02d",tm.tm_mday);
    char *filename = concat(7,"resource/",year,"-",month,"-",day,"log.json");
    //printf(filename);

    // GET DATA FROM JSON FILE
    fp = fopen(filename,"r");
    fread(buffer, 1024, 1, fp);
    fclose(fp);

    parsed_json = json_tokener_parse(buffer);
    json_object_object_get_ex(parsed_json, key, &data);
    free(filename);
    return data;
}

int main(int argc, char **argv)
{
    char confd_port[16];
    struct addrinfo hints;
    struct addrinfo *addr = NULL;
    int debuglevel = CONFD_SILENT;
    int i;
    int c;
    char *p, *dname;
    struct confd_notification_stream_cbs ncb;
    struct pollfd set[3];
    int ret;

    snprintf(confd_port, sizeof(confd_port), "%d", CONFD_PORT);
    memset(&hints, 0, sizeof(hints));
    hints.ai_family = PF_UNSPEC;
    hints.ai_socktype = SOCK_STREAM;

    while ((c = getopt(argc, argv, "dtprc:")) != -1) {
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
                p = confd_port;
            if (getaddrinfo(optarg, p, &hints, &addr) != 0) {
                if (p != confd_port) {
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
        default:
            fprintf(stderr,
                    "Usage: %s [-dtpr] [-c address[/port]]\n",
                    argv[0]);
            exit(1);
        }
    }

    if (addr == NULL &&
        ((i = getaddrinfo("127.0.0.1", confd_port, &hints, &addr)) != 0))
        /* "Can't happen" */
        confd_fatal("%s: Failed to get address for ConfD: %s\n",
                    argv[0], gai_strerror(i));
    if ((dname = strrchr(argv[0], '/')) != NULL)
        dname++;
    else
        dname = argv[0];
    /* Init library */
    confd_init(dname, stderr, debuglevel);

    if ((dctx = confd_init_daemon(dname)) == NULL)
        confd_fatal("Failed to initialize ConfD\n");
    if ((ctlsock = get_ctlsock(addr)) < 0)
        confd_fatal("Failed to connect to ConfD\n");
    if ((workersock = get_workersock(addr)) < 0)
        confd_fatal("Failed to connect to ConfD\n");

    memset(&ncb, 0, sizeof(ncb));
    ncb.fd = workersock;
    ncb.get_log_times = NULL;
    ncb.replay = NULL;
    strcpy(ncb.streamname, "I2NSF-Monitoring");
    ncb.cb_opaque = NULL;
    if (confd_register_notification_stream(dctx, &ncb, &live_ctx) != CONFD_OK) {
        confd_fatal("Couldn't register stream %s\n", ncb.streamname);
    }
    if (confd_register_done(dctx) != CONFD_OK) {
        confd_fatal("Failed to complete registration \n");
    }

    printf("notifier started\n");
    fflush(stdout);

    bool over = false;
    while (1){
    char *sep = " \n";
    FILE *p = popen("free", "r");
    char buf[BUFSIZ];
    struct ifentry *ife = NULL;
    while (fgets(&buf[0],BUFSIZ,p)!=NULL)
    {
      char *cp = strtok(&buf[0],sep);
      if (strcmp(cp, "total")!=0){
        // reads text until newline is encountered
        cp = strtok(NULL, sep);
        int total = atoi(cp);

        cp = strtok(NULL, sep);
        int used = atoi(cp);

        cp = strtok(NULL, sep);
        int free = atoi(cp);

        float percent = (float)used * 100 / total;
        if ((percent > 80)&&(!over)) {
          over = true;
          send_notif_monitoring(nsfmi_MEM_USAGE_ALARM,percent,80,nsfmi_subscription,nsfmi_on_change,"time_based_firewall",nsfmi_high,"Memory Usage Exceeded The Threshold");
	  printf("SENDING ALARM DATA\n");
        }
        else if ((over)&&(percent<=80)) {
	  system("python log.py");
          FILE *fp;
          char buffer[1024];
          struct json_object *parsed_json;
          struct json_object *status;
          struct json_object *cpu_usage;
          struct json_object *memory_usage;
          struct json_object *disk_usage;
          struct json_object *disk_left;
          struct json_object *in_traffic_speed;
          struct json_object *out_traffic_speed;
          over = false;
          
          status = getJson("system_status");
          cpu_usage = getJson("cpu_usage");
          memory_usage = getJson("memory_usage");
          disk_usage = getJson("disk_usage");
          disk_left = getJson("disk_left");
          in_traffic_speed = getJson("in_traffic_speed");
          out_traffic_speed = getJson("out_traffic_speed");

          send_notif_resources(json_object_get_string(status),json_object_get_int(cpu_usage),json_object_get_int(memory_usage),json_object_get_int(disk_usage),json_object_get_int(disk_left), json_object_get_int(in_traffic_speed),json_object_get_int(out_traffic_speed),"time_based_firewall");
	  printf("SENDING RESOURCES DATA\n");
        }
        break;
      }

    }
    fclose(p);
    }
    return 0;
}


/*    while (1) {
        set[0].fd = ctlsock;
        set[0].events = POLLIN;
        set[0].revents = 0;

        set[1].fd = 0;
        set[1].events = POLLIN;
        set[1].revents = 0;

        switch (poll(set, 2, -1)) {
        case -1:
            break;

        default:
            //Check for I/O
            if (set[0].revents & POLLIN) { //ctlsock
                if ((ret = confd_fd_ready(dctx, ctlsock)) == CONFD_EOF) {
                    confd_fatal("Control socket closed\n");
                } else if (ret == CONFD_ERR &&
                           confd_errno != CONFD_ERR_EXTERNAL) {
                    confd_fatal("Error on control socket request: "
                                "%s (%d): %s\n", confd_strerror(confd_errno),
                                confd_errno, confd_lasterr());
                }
            }
            if (set[1].revents & (POLLIN|POLLHUP)) { // stdin
         char c;
                if (!read(0, &c, 1))
                    exit(0);
                switch (c) {
                case 'n':
                    printf("sending monitoring notification\n");
                    send_notif_monitoring(nsfmi_MEM_USAGE_ALARM,91,90,nsfmi_subscription,nsfmi_on_change,"time_based_firewall",nsfmi_high);
                    break;
                case '\n':
                    break;
                default:
                    printf("unknown character <%c>\n", c);
                    break;
                }
            }
        }
    }
}
*/

