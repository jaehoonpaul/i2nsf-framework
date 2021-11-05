/*********************************************************************
 * ConfD Subscriber intro example
 * Implements a DHCP server adapter
 *
 * (C) 2005-2007 Tail-f Systems
 * Permission to use this code as a starting point hereby granted
 *
 * See the README file for more information
 ********************************************************************/

#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <sys/poll.h>
#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include <stdio.h>

#include <confd_lib.h>
#include <confd_cdb.h>

/* include generated file */
#include "ietf-i2nsf-capability.h"
#include "ietf-i2nsf-reg-interface.h"

/********************************************************************/

int rule_id = 40000;
int confd_start = 1;
char exist_nsf[500] = "";


static int read_conf(struct sockaddr_in *addr)
{
    FILE *fp;
    int nsf_num, rule_num;
    int i, j, k;
    int rsock;
    char nsf_name[300];
    confd_value_t *time_capa;
    int time_capa_num;
    confd_value_t *ipv4_capa;
    int ipv4_capa_num;
    confd_value_t *tcp_capa;
    int tcp_capa_num;
    confd_value_t *http_capa;
    int http_capa_num;
    confd_value_t *voice_capa;
    int voice_capa_num;
    confd_value_t *antiddos_capa;
    int antiddos_capa_num;
    confd_value_t *ingress_action_capa;
    int ingress_action_capa_num;
     confd_value_t *egress_action_capa;
    int egress_action_capa_num;
    char *temp_exist_nsf_name;
    char temp_exist_nsf[300];
    int exist = 0;
    char nsf_capa[1000];
    u_int16_t processing_average;
    u_int16_t processing_peak;
    u_int16_t outbound_average;
    u_int16_t outbound_peak;
    u_int16_t inbound_average;
    u_int16_t inbound_peak;
    char str_processing_average[100];
    char str_processing_peak[100];
    char str_outbound_average[100];
    char str_outbound_peak[100];
    char str_inbound_average[100];
    char str_inbound_peak[100];

    if ((rsock = socket(PF_INET, SOCK_STREAM, 0)) < 0 )
        confd_fatal("Failed to open socket\n");

    if (cdb_connect(rsock, CDB_READ_SOCKET, (struct sockaddr*)addr,
                      sizeof (struct sockaddr_in)) < 0)
        return CONFD_ERR;
    if (cdb_start_session(rsock, CDB_RUNNING) != CONFD_OK)
        return CONFD_ERR;
    cdb_set_namespace(rsock, iiregi__ns);

    cdb_cd(rsock, "i2nsf-nsf-registrations");
    nsf_num = cdb_num_instances(rsock, "i2nsf-nsf-capability-registration");

    printf("NSF Num: %d\n", nsf_num);

    for (j = 0; j < nsf_num; j++) {
        printf("Current Num: %d\n", j);
        //cdb_get_str(rsock, &nsf_name[0], sizeof(nsf_name), "nsf-name");
        //printf("nsf-name: %s\n\n",nsf_name);

	for (k = 0; k < 1000; k++) {
		nsf_capa[k] = " ";
	} 	

        cdb_get_str(rsock, &nsf_name[0], sizeof(nsf_name), "i2nsf-nsf-capability-registration[%d]/nsf-name",j);
        printf("nsf-name: %s\n\n",nsf_name);
	strncpy(temp_exist_nsf,exist_nsf,sizeof(temp_exist_nsf));
	temp_exist_nsf_name = strtok(temp_exist_nsf, ",");
	while(temp_exist_nsf_name != NULL)
	{
		printf("\nTest: %s\n", temp_exist_nsf_name);
		if (strncmp(temp_exist_nsf_name,nsf_name,sizeof(nsf_name)) == 0) {
			exist = 1;
		}
		temp_exist_nsf_name = strtok(NULL,",");
			
	}
	if (exist == 1) {
		exist = 0;
		continue;
	}

        cdb_pushd(rsock, "i2nsf-nsf-capability-registration[%d]", j);
	cdb_get_str(rsock, &nsf_name[0], sizeof(nsf_name), "nsf-name");
	strncat(exist_nsf,nsf_name,sizeof(nsf_name));
	strncat(exist_nsf,", ",1);
	printf("Exist NSF: %s\n\n", exist_nsf);
	strncpy(nsf_capa,"\nnsf-name: ",sizeof("\nnsf-name: "));
	strncat(nsf_capa,nsf_name,sizeof(nsf_name));
//	strncat(nsf_capa,",",1);


        cdb_cd(rsock, "nsf-capability-info/i2nsf-capability");
	if (cdb_get_list(rsock, &time_capa, &time_capa_num, "time-capabilities") == CONFD_ERR) {
		time_capa_num = 0;
	}
	else {
		strncat(nsf_capa,"\ntime-capabilities: ",sizeof("\ntime-capabilities: "));	
		for (i = 0; i < time_capa_num; i++) {
	                switch(CONFD_GET_ENUM_VALUE(&time_capa[i])) {
	                        case iiregi_absolute_time:
	                                printf("Time Capa: %s\n", "absolute-time");
					strncat(nsf_capa,"absolute-time",sizeof("absolute-time"));
	                                break;
	                        case iiregi_periodic_time:
	                                printf("Time Capa: %s\n", "periodic-time");
					strncat(nsf_capa,"periodic-time",sizeof("periodic-time"));
                        	        break;
                	}
			if (i < (time_capa_num-1)) {
				strncat(nsf_capa,",",1);
			}
		}
	}


        cdb_cd(rsock, "condition-capabilities/generic-nsf-capabilities");
	if (cdb_get_list(rsock, &ipv4_capa, &ipv4_capa_num, "ipv4-capa") == CONFD_ERR) {
		ipv4_capa_num = 0;
	} else {
		strncat(nsf_capa,"\nipv4-capa: ",sizeof("\nipv4-capa: "));	
		for (i = 0; i < ipv4_capa_num; i++) {
                	switch(CONFD_GET_IDENTITYREF(&ipv4_capa[i]).id) {
        	                case iicapa_ipv4_protocol:
	                                printf("IPv4 Capa: %s\n", "ipv4-protocol");
					strncat(nsf_capa,"ipv4-protocol",sizeof("ipv4-protocol"));
                	                break;
        	                case iicapa_exact_ipv4_address:
	                                printf("IPv4 Capa: %s\n", "exact-ipv4-address");
					strncat(nsf_capa,"exact-ipv4-address",sizeof("exact-ipv4-address"));
                	                break;
        	                case iicapa_range_ipv4_address:
	                                printf("IPv4 Capa: %s\n", "range-ipv4-address");
					strncat(nsf_capa,"range-ipv4-address",sizeof("range-ipv4-address"));
                        	        break;
        	        }
			if (i < (ipv4_capa_num-1)) {
				strncat(nsf_capa,",",1);
			}

		}

	}

	if (cdb_get_list(rsock, &tcp_capa, &tcp_capa_num, "tcp-capa") == CONFD_ERR) {
		tcp_capa_num = 0;
	} else {
		strncat(nsf_capa,"\ntcp-capa: ",sizeof("\ntcp-capa: "));	
		for (i = 0; i < tcp_capa_num; i++) {
                	switch(CONFD_GET_IDENTITYREF(&tcp_capa[i]).id) {
        	                case iicapa_exact_tcp_port_num:
	                                printf("TCP Capa: %s\n", "exact-tcp-port-num");
					strncat(nsf_capa,"exact-tcp-port-num",sizeof("exact-tcp-port-num"));
                	                break;
        	                case iicapa_range_tcp_port_num:
	                                printf("TCP Capa: %s\n", "range-tcp-port-num");
					strncat(nsf_capa,"range-tcp-port-num",sizeof("range-tcp-port-num"));
                	                break;
        	        }
			if (i < (tcp_capa_num-1)) {
				strncat(nsf_capa,",",1);
			}

		}
	}

        cdb_cd(rsock, "../advanced-nsf-capabilities");
	if (cdb_get_list(rsock, &http_capa, &http_capa_num, "http-capa") == CONFD_ERR) {
		http_capa_num = 0;
	} else {
		strncat(nsf_capa,"\nhttp-capa: ",sizeof("\nhttp-capa: "));	
		for (i = 0; i < http_capa_num; i++) {
                	switch(CONFD_GET_IDENTITYREF(&http_capa[i]).id) {
        	                case iicapa_url:
	                                printf("HTTP Capa: %s\n", "url");
					strncat(nsf_capa,"url",sizeof("url"));
                	                break;
        	        }
			if (i < (http_capa_num-1)) {
				strncat(nsf_capa,",",1);
			}

		}
	}

	if (cdb_get_list(rsock, &voice_capa, &voice_capa_num, "voip-volte-capa") == CONFD_ERR) {
		voice_capa_num = 0;
	} else {
		strncat(nsf_capa,"\nvoip-volte-capa: ",sizeof("\nvoip-volte-capa: "));	
		for (i = 0; i < voice_capa_num; i++) {
        	        switch(CONFD_GET_IDENTITYREF(&voice_capa[i]).id) {
                	        case iicapa_voice_id:
                        	        printf("VoIP/VoLTE Capa: %s\n", "voice-id");
					strncat(nsf_capa,"voice-id",sizeof("voice-id"));
        	                        break;
                	}
			if (i < (voice_capa_num-1)) {
				strncat(nsf_capa,",",1);
			}
		}
	}

	if (cdb_get_list(rsock, &antiddos_capa, &antiddos_capa_num, "antiddos-capa") == CONFD_ERR) {
		antiddos_capa_num = 0;
	} else {
		strncat(nsf_capa,"\nantiddos-capa: ",sizeof("\nantiddos-capa: "));	
		for (i = 0; i < antiddos_capa_num; i++) {
        	        switch(CONFD_GET_IDENTITYREF(&antiddos_capa[i]).id) {
                	        case iicapa_http_flood_action:
                        	        printf("Anti DDoS Capa: %s\n", "http-flood-action");
					strncat(nsf_capa,"http-flood-action",sizeof("http-flood-action"));
        	                        break;
                	        case iicapa_https_flood_action:
                        	        printf("Anti DDoS Capa: %s\n", "https-flood-action");
					strncat(nsf_capa,"https-flood-action",sizeof("https-flood-action"));
        	                        break;

                	}
			if (i < (antiddos_capa_num-1)) {
				strncat(nsf_capa,",",1);
			}

		}
	}



        cdb_cd(rsock, "../../action-capabilities");
	if(cdb_get_list(rsock, &ingress_action_capa, &ingress_action_capa_num, "ingress-action-capa") == CONFD_ERR) {
		ingress_action_capa_num = 0;
	} else {
		strncat(nsf_capa,"\ningress-action-capa: ",sizeof("\ningress-action-capa: "));	

		for (i = 0; i < ingress_action_capa_num; i++) {
        	        switch(CONFD_GET_IDENTITYREF(&ingress_action_capa[i]).id) {
                	        case iicapa_pass:
                        	        printf("Ingress Action Capa: %s\n", "pass");
					strncat(nsf_capa,"pass",sizeof("pass"));
        	                        break;
                	        case iicapa_drop:
                        	        printf("Ingress Action Capa: %s\n", "drop");
					strncat(nsf_capa,"drop",sizeof("drop"));
        	                        break;
                	        case iicapa_alert:
                        	        printf("Ingress Action Capa: %s\n", "alert");
					strncat(nsf_capa,"alert",sizeof("alert"));
        	                        break;
                	}
			if (i < (ingress_action_capa_num-1)) {
				strncat(nsf_capa,",",1);
			}

		}

	}

	if(cdb_get_list(rsock, &egress_action_capa, &egress_action_capa_num, "egress-action-capa") == CONFD_ERR) {
		egress_action_capa_num = 0;
	} else {
		strncat(nsf_capa,"\negress-action-capa: ",sizeof("\negress-action-capa: "));	

		for (i = 0; i < egress_action_capa_num; i++) {
        	        switch(CONFD_GET_IDENTITYREF(&egress_action_capa[i]).id) {
                	        case iicapa_pass:
                        	        printf("Egress Action Capa: %s\n", "pass");
					strncat(nsf_capa,"pass",sizeof("pass"));
        	                        break;
                	        case iicapa_drop:
                        	        printf("Egress Action Capa: %s\n", "drop");
					strncat(nsf_capa,"drop",sizeof("drop"));
        	                        break;
                	        case iicapa_alert:
                        	        printf("Egress Action Capa: %s\n", "alert");
					strncat(nsf_capa,"alert",sizeof("alert"));
        	                        break;
                	}
			if (i < (egress_action_capa_num-1)) {
				strncat(nsf_capa,",",1);
			}

		}
	}

        cdb_cd(rsock, "../../nsf-performance-capability/");

	strncat(nsf_capa,"\nprocessing: ",sizeof("\nprocessing: "));	
	cdb_get_u_int16(rsock, &processing_average, "processing/processing-average");
	printf("Processing Average: %d\n", processing_average);
	sprintf(str_processing_average,"%d", processing_average);
	strncat(nsf_capa,str_processing_average,sizeof(str_processing_average));
	strncat(nsf_capa,",",1);

	cdb_get_u_int16(rsock, &processing_peak, "processing/processing-peak");
	printf("Processing Peak: %d\n", processing_peak);
	sprintf(str_processing_peak,"%d", processing_peak);
	strncat(nsf_capa,str_processing_peak,sizeof(str_processing_peak));


	strncat(nsf_capa,"\nBandwidth Outbound: ",sizeof("\nBandwidth Outbound: "));	
	cdb_get_u_int16(rsock, &outbound_average, "bandwidth/outbound/outbound-average");
	printf("Outbound Average: %d\n", outbound_average);
	sprintf(str_outbound_average,"%d", outbound_average);
	strncat(nsf_capa,str_outbound_average,sizeof(str_outbound_average));
	strncat(nsf_capa,",",1);

	cdb_get_u_int16(rsock, &outbound_peak, "bandwidth/outbound/outbound-peak");
	printf("Outbound Peak: %d\n", outbound_peak);
	sprintf(str_outbound_peak,"%d", outbound_peak);
	strncat(nsf_capa,str_outbound_peak,sizeof(str_outbound_peak));


	strncat(nsf_capa,"\nBandwidth Inbound: ",sizeof("\nBandwidth Inbound: "));	
	cdb_get_u_int16(rsock, &inbound_average, "bandwidth/inbound/inbound-average");
	printf("Inbound Average: %d\n", inbound_average);
	sprintf(str_inbound_average,"%d", inbound_average);
	strncat(nsf_capa,str_inbound_average,sizeof(str_inbound_average));
	strncat(nsf_capa,",",1);

	cdb_get_u_int16(rsock, &inbound_peak, "bandwidth/inbound/inbound-peak");
	printf("Inbound Peak: %d\n", inbound_peak);
	sprintf(str_inbound_peak,"%d", inbound_peak);
	strncat(nsf_capa,str_inbound_peak,sizeof(str_inbound_peak));

	printf("\n\nNSF Capa: %s\n\n", nsf_capa);

        int client_fd,len;
        struct sockaddr_in client_addr;

        client_fd = socket(PF_INET, SOCK_STREAM, 0);

        client_addr.sin_addr.s_addr = inet_addr("10.0.0.17"); /* EDIT THE IP ADDRESS WITH YOUR SECURITY CONTROLLER */
        client_addr.sin_family = AF_INET;
        client_addr.sin_port = htons(55552);

        if(connect(client_fd,(struct sockaddr *)&client_addr, sizeof(client_addr)) == -1)
        {
                printf("Can't connect\n");
                close(client_fd);
                return -1;
        }

        send(client_fd, (char *)nsf_capa, strlen(nsf_capa)+1, 0);

        close(client_fd);
        printf("\nSend: %s\n", nsf_capa);
        printf("\n\n\n", nsf_capa);


        //do_rule(rsock, fp);
        cdb_popd(rsock);
    }

        /*if (confd_start == 1) {
                sleep(10);
                confd_start = 0;
        }
        system("sudo /usr/bin/suricatasc -c reload-rules /var/run/suricata/suricata-command.socket");
*/
    printf("Success\n");

    return cdb_close(rsock);
}

/********************************************************************/

int start_confd(void)
{
    struct sockaddr_in addr;
    int subsock;
    int status;
    int spoint;

    addr.sin_addr.s_addr = inet_addr("127.0.0.1");
    addr.sin_family = AF_INET;
    addr.sin_port = htons(CONFD_PORT);

    confd_init("firewall", stderr, CONFD_TRACE);

    /*
     * Setup subscriptions
     */
    if ((subsock = socket(PF_INET, SOCK_STREAM, 0)) < 0 )
        confd_fatal("Failed to open socket\n");

    if (cdb_connect(subsock, CDB_SUBSCRIPTION_SOCKET, (struct sockaddr*)&addr,
                      sizeof (struct sockaddr_in)) < 0)
        confd_fatal("Failed to cdb_connect() to confd \n");

    if ((status = cdb_subscribe(subsock, 3, iiregi__ns, &spoint, "/i2nsf-nsf-registrations"))
        != CONFD_OK) {
        fprintf(stderr, "Terminate: subscribe %d\n", status);
        exit(0);
    }
    if (cdb_subscribe_done(subsock) != CONFD_OK)
        confd_fatal("cdb_subscribe_done() failed");
    printf("Subscription point = %d\n", spoint);

    /*
     * Read initial config
     */
    if ((status = read_conf(&addr)) != CONFD_OK) {
        fprintf(stderr, "Terminate: read_conf %d\n", status);
        exit(0);
    }
    /* This is the place to HUP the daemon */

    while (1) {
        static int poll_fail_counter=0;
        struct pollfd set[1];

        set[0].fd = subsock;
        set[0].events = POLLIN;
        set[0].revents = 0;

        if (poll(&set[0], 1, -1) < 0) {
            perror("Poll failed:");
            if(++poll_fail_counter < 10)
                continue;
            fprintf(stderr, "Too many poll failures, terminating\n");
            exit(1);
        }

        poll_fail_counter = 0;
        if (set[0].revents & POLLIN) {
            int sub_points[1];
            int reslen;


            if ((status = cdb_read_subscription_socket(subsock,
                                                       &sub_points[0],
                                                       &reslen)) != CONFD_OK) {
                fprintf(stderr, "terminate sub_read: %d\n", status);
                exit(1);
            }
            if (reslen > 0) {
                if ((status = read_conf(&addr)) != CONFD_OK) {
                    fprintf(stderr, "Terminate: read_conf %d\n", status);
                    exit(1);
                }
            }

            fprintf(stderr, "Read new config, updating dhcpd config \n");
            /* this is the place to HUP the daemon */

            if ((status = cdb_sync_subscription_socket(subsock,
                                                       CDB_DONE_PRIORITY))
                != CONFD_OK) {
                fprintf(stderr, "failed to sync subscription: %d\n", status);
                exit(1);
            }
        }
    }
}

/********************************************************************/

