/*
 * Copyright 2006 Tail-F Systems AB
 */


#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <string.h>
#include <stdlib.h>
#include <unistd.h>
#include <stdio.h>
#include <time.h>
#include <sys/time.h>
#include <signal.h>

#include <confd_lib.h>
#include <confd_cdb.h>
#include "load.h"

#define INTERVAL    60
#define MAX_SAMPLES (86400/interval)

#define OK(rval) do {                                                   \
        if ((rval) != CONFD_OK)                                         \
            confd_fatal("get_load: error not CONFD_OK: %d : %s\n",      \
                        confd_errno, confd_lasterr());                  \
    } while (0);


static volatile int do_get_load = 0;

static void catch_alarm(int sig)
{
    do_get_load++;
}


static void get_load(struct sockaddr_in *addr, int max_samples)
{
    time_t now = time(NULL);
    struct tm *tm = localtime(&now);
    struct confd_datetime dt;
    int sock;
    confd_value_t timeval, cpu;
    static double last_up = 0.0, last_idle = 0.0;
    double up, idle;
    struct confd_decimal64 util;
    FILE *proc;
    char buf[BUFSIZ];
    char *p;
    int n;

    dt.year = tm->tm_year + 1900; dt.month = tm->tm_mon + 1;
    dt.day = tm->tm_mday; dt.hour = tm->tm_hour;
    dt.min = tm->tm_min; dt.sec = tm->tm_sec;
    dt.micro = 0; dt.timezone = CONFD_TIMEZONE_UNDEF;
    CONFD_SET_DATETIME(&timeval, dt);

    if ((sock = socket(PF_INET, SOCK_STREAM, 0)) < 0)
        confd_fatal("Failed to create socket");
    OK(cdb_connect(sock, CDB_DATA_SOCKET, (struct sockaddr *)addr,
                   sizeof(struct sockaddr_in)));
    OK(cdb_start_session(sock, CDB_OPERATIONAL));
    OK(cdb_set_namespace(sock, load__ns));
    OK(cdb_create(sock, "/load/sample{%x}", &timeval));
    OK(cdb_cd(sock, "/load/sample{%x}", &timeval));

    util.fraction_digits = 1;
    util.value = 0;
    /* /proc/uptime only in linux :p */
    if ((proc = fopen("/proc/uptime", "r")) != NULL) {
        if (fgets(buf, sizeof(buf), proc) != NULL) {
            if (sscanf(buf, "%lf %lf", &up, &idle) == 2) {
                util.value =
                    (int64_t)(1000.0*(1.0 - (idle - last_idle)/(up - last_up)));
                /* avoid spurious negative util when ~= 0
                   (due to imprecise numbers in /proc/uptime) */
                if (util.value < 0)
                    util.value = 0;
                last_idle = idle; last_up = up;
            }
        }
        fclose(proc);
    }

    CONFD_SET_DECIMAL64(&cpu, util);
    OK(cdb_set_elem(sock, &cpu, "cpu_percent"));

    if ((proc = popen("uptime", "r")) != NULL) {
        if (fgets(buf, sizeof(buf), proc) != NULL) {
            char *last;
            char *sep = ", \t\n";

            strtok_r(buf, sep, &last);
            while ((p = strtok_r(NULL, sep, &last)) != NULL)
                /* "averages:" on bsd, "average:" on linux/solaris */
                if (strncmp(p, "average", 7) == 0)
                    break;
            if ((p = strtok_r(NULL, sep, &last)) != NULL) {
                OK(cdb_create(sock, "average{1}"));
                OK(cdb_set_elem2(sock, p, "average{1}/load"));
            }
            if ((p = strtok_r(NULL, sep, &last)) != NULL) {
                OK(cdb_create(sock, "average{5}"));
                OK(cdb_set_elem2(sock, p, "average{5}/load"));
            }
            if ((p = strtok_r(NULL, sep, &last)) != NULL) {
                OK(cdb_create(sock, "average{15}"));
                OK(cdb_set_elem2(sock, p, "average{15}/load"));
            }
        }
        pclose(proc);
    }

    n = cdb_num_instances(sock, "/load/sample");
    while (n-- > max_samples)
        OK(cdb_delete(sock, "/load/sample[0]")); /* delete oldest */

    OK(cdb_close(sock));
}


int main(int argc, char **argv)
{
    int interval = 0;
    int max_samples = 0;
    struct sockaddr_in addr;
    time_t now;
    struct tm *tm;
    struct itimerval timer;

    if (argc > 1)
        interval = atoi(argv[1]);
    if (argc > 2)
        max_samples = atoi(argv[2]);
    if (argc > 3)
        printf("PID = %d\n", getpid());
    if (interval == 0)
        interval = INTERVAL;
    if (max_samples == 0)
        max_samples = MAX_SAMPLES;

    addr.sin_addr.s_addr = inet_addr("127.0.0.1");
    addr.sin_family = AF_INET;
    addr.sin_port = htons(CONFD_PORT);


    confd_init(argv[0], stderr, CONFD_TRACE);
    OK(confd_load_schemas((struct sockaddr*)&addr, sizeof(struct sockaddr_in)));
    signal(SIGALRM, catch_alarm);
    /* start at next multiple of interval */
    now = time(NULL);
    tm = localtime(&now);
    timer.it_value.tv_sec = interval - tm->tm_sec % interval;
    timer.it_value.tv_usec = 0;
    timer.it_interval.tv_sec = interval;
    timer.it_interval.tv_usec = 0;
    setitimer(ITIMER_REAL, &timer, NULL);

    while (1) {
        pause();
        if (do_get_load) {
            do_get_load = 0;
            get_load(&addr, max_samples);
        }
    }
}
