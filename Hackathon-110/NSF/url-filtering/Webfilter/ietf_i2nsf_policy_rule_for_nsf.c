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
#include "ietf-i2nsf-policy-rule-for-nsf.h"

/********************************************************************/

int rule_id = 40000;
int confd_start = 1;
static void do_rule(int rsock, FILE *fp)
{
//   struct in_addr *ip_list;
//   struct confd_datetime date_time;
   char date_time[BUFSIZ];

   char temp_rule[30000];
   char temp_itoa[300];
   int i,j;
   char rule_name[BUFSIZ];
   char common_ip_addr[10000];
   char str_src_start_ip_addr[50];
   char str_src_end_ip_addr[50];
   char *start_ptr;
   char str_start_ptr[100];
   char *end_ptr;
   char str_end_ptr[100];
   char temp_str[10];
   int int_start_ptr;
   int int_end_ptr;
   int url_content_num;
   confd_value_t *url_content;
   struct in_addr src_start_ip_addr;
   struct in_addr src_end_ip_addr;
   confd_value_t *advanced_action;
   int advanced_action_num;
   struct confd_identityref ingress_action;
   struct confd_identityref egress_action;
   char str_content[100];
   char *str_ingress_action;
   char *str_egress_action;


   for (i =0; i <30000; i++) {
         temp_rule[i] = NULL;
   }


        cdb_get_str(rsock, &rule_name[0], BUFSIZ, "rule-name");
        printf("rule-name: %s\n",rule_name);



        cdb_cd(rsock, "condition-clause-container/packet-security-http-condition");
	cdb_get_list(rsock, &url_content, &url_content_num, "pkt-sec-url-content");
	for (i = 0; i < url_content_num; i++) {
		printf("URL Content: %s\n", CONFD_GET_BUFPTR(&url_content[i]));
	}


        cdb_cd(rsock, "../../action-clause-container/packet-action");
        if (cdb_get_identityref(rsock, &ingress_action, "ingress-action") == 0) {

		switch(ingress_action.id) {
			case iiprfn_pass:
				printf("Ingress action: %s\n", "pass");
				str_ingress_action = "pass";
				break;
			case iiprfn_drop:
				printf("Ingress action: %s\n", "drop");
				str_ingress_action = "drop";
				break;
			case iiprfn_reject:
				printf("Ingress action: %s\n", "reject");
				str_ingress_action = "reject";
				break;
			case iiprfn_alert:
				printf("Ingress action: %s\n", "alert");
				str_ingress_action = "alert";
				break;
			case iiprfn_mirror:
				printf("Ingress action: %s\n", "mirror");
				str_ingress_action = "mirror";
				break;
		}
	}

        if (cdb_get_identityref(rsock, &egress_action, "egress-action") == 0) {

		switch(egress_action.id) {
			case iiprfn_pass:
				printf("Egress action: %s\n", "pass");
				str_egress_action = "pass";
				break;
			case iiprfn_drop:
				printf("Egress action: %s\n", "drop");
				str_egress_action = "drop";
				break;
			case iiprfn_reject:
				printf("Egress action: %s\n", "reject");
				str_egress_action = "reject";
				break;
			case iiprfn_alert:
				printf("Egress action: %s\n", "alert");
				str_egress_action = "alert";
				break;
			case iiprfn_mirror:
				printf("Egress action: %s\n", "mirror");
				str_egress_action = "mirror";
				break;
		}
	}

	for (i = 0; i < url_content_num; i++) {
		printf("URL Content: %s\n", CONFD_GET_BUFPTR(&url_content[i]));
	}

	
	




        for (i = 0; i < url_content_num; i++) {
                strncat(temp_rule, str_egress_action, sizeof(temp_rule));
                printf("\n%s\n", str_egress_action);
                strncat(temp_rule," udp any any -> any any", sizeof(temp_rule));
                strncat(temp_rule, " (msg:\"", sizeof(temp_rule));
                strncat(temp_rule, rule_name, sizeof(temp_rule));
                strncat(temp_rule, "\"; content:\"", sizeof(temp_rule));
                strncat(temp_rule, CONFD_GET_BUFPTR(&url_content[i]), sizeof(temp_rule));
                strncat(temp_rule, "\"; nocase; sid:", sizeof(temp_rule));
                sprintf(temp_itoa, "%d", rule_id);
                strncat(temp_rule, temp_itoa, sizeof(temp_rule));
                strncat(temp_rule, "; rev:1;)\n", sizeof(temp_rule));
                rule_id++;
        }

	printf("Rule: %s", temp_rule);
        fputs(temp_rule,fp);


}

static int read_conf(struct sockaddr_in *addr)
{
    FILE *fp, *fp_temp, *fp_suricata_yaml;
        int policy_num, rule_num;
    int i, j;
    int rsock;
        char temp_policy_name[100];
        char temp_policy_file_location[100];
        char temp_yaml_content[100];
        char temp_cp[100];
        char policy_name[BUFSIZ];
        int temp_fp_location;
        


    if ((rsock = socket(PF_INET, SOCK_STREAM, 0)) < 0 )
        confd_fatal("Failed to open socket\n");

    if (cdb_connect(rsock, CDB_READ_SOCKET, (struct sockaddr*)addr,
                      sizeof (struct sockaddr_in)) < 0)
        return CONFD_ERR;
    if (cdb_start_session(rsock, CDB_RUNNING) != CONFD_OK)
        return CONFD_ERR;
    cdb_set_namespace(rsock, iiprfn__ns);

        system("cp /home/ubuntu/suricata.yaml /etc/suricata/suricata.yaml");

        fp_temp = fopen("/etc/suricata/suricata.yaml.temp","w+");
        fp_suricata_yaml = fopen("/etc/suricata/suricata.yaml","r+");

        while(!feof(fp_suricata_yaml)) {
                fgets(temp_yaml_content, sizeof(temp_yaml_content), fp_suricata_yaml);
                if(strstr(temp_yaml_content, "rule-files:") != NULL) {
                        printf("Succes\n");
                        temp_fp_location = ftell(fp_suricata_yaml);

                        while(!feof(fp_suricata_yaml)) {
                                fgets(temp_yaml_content, sizeof(temp_yaml_content), fp_suricata_yaml);
                                fputs(temp_yaml_content, fp_temp);
                        }
                }
        }

        fseek(fp_suricata_yaml, temp_fp_location, SEEK_SET);
        fseek(fp_temp, 0, SEEK_SET);


        policy_num = cdb_num_instances(rsock, "i2nsf-security-policy/system-policy");
        printf("Policy Num: %d\n", policy_num);
        for(i = 0; i < policy_num; i++) {
                cdb_pushd(rsock, "i2nsf-security-policy/system-policy[%d]", i);
                cdb_get_str(rsock, &policy_name[0], BUFSIZ, "system-policy-name");
                strcpy(policy_name, policy_name);
                strncpy(temp_policy_name," - ", sizeof(temp_policy_name));
                strcat(temp_policy_name, policy_name);
                strncat(temp_policy_name,".rules\n", sizeof(temp_policy_name));
                fputs(temp_policy_name, fp_suricata_yaml);


                strncpy(temp_policy_file_location,"/etc/suricata/rules/", sizeof(temp_policy_file_location));
                strncat(temp_policy_file_location, policy_name, sizeof(temp_policy_file_location));
                strncat(temp_policy_file_location,".rules\n", sizeof(temp_policy_file_location));
                printf("System policy-name: %s\n",temp_policy_file_location);



            if ((fp = fopen("test.tmp", "w")) == NULL) {
                    cdb_close(rsock);
                        return CONFD_ERR;
            }


                rule_num = cdb_num_instances(rsock, "rules");
                printf("Rule Num: %d\n", rule_num);

                for (j = 0; j < rule_num; j++) {
                        cdb_pushd(rsock, "rules[%d]", j);
                        do_rule(rsock, fp);
                        cdb_popd(rsock);
                }
              cdb_popd(rsock);

              fclose(fp);

                strncpy(temp_cp, "cp test.tmp ", sizeof(temp_cp));
                strncat(temp_cp, temp_policy_file_location, sizeof(temp_cp));
                system(temp_cp);
        }

        while(!feof(fp_temp)) {
                fgets(temp_yaml_content, sizeof(temp_yaml_content), fp_temp);
                fputs(temp_yaml_content, fp_suricata_yaml);
        }

        fclose(fp_suricata_yaml);
        fclose(fp_temp);

        /*if (confd_start == 1) {
                sleep(10);
                confd_start = 0;
        }*/

        system("sudo /usr/bin/suricatasc -c shutdown");
        system("sudo rm /var/run/suricata.pid");

        sleep(2);
        system("sudo /usr/bin/suricata -D -c /etc/suricata/suricata.yaml -q 0");


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

    if ((status = cdb_subscribe(subsock, 3, iiprfn__ns, &spoint, "/i2nsf-security-policy"))
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

