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
   int range_src_ip_num;
   struct in_addr src_start_ip_addr;
   struct in_addr src_end_ip_addr;
   confd_value_t *advanced_action;
   int advanced_action_num;

   for (i=0; i<30000; i++) {
        temp_rule[i] = NULL;
   }
   

        cdb_get_str(rsock, &rule_name[0], BUFSIZ, "rule-name");
        printf("rule-name: %s\n",rule_name);

        cdb_cd(rsock, "time-zone/absolute-time-zone");
        cdb_get_str(rsock, &date_time[0], BUFSIZ, "start-time");
        printf("Start TIme: %s\n", date_time);
        cdb_get_str(rsock, &date_time[0], BUFSIZ, "end-time");
        printf("End Time: %s\n", date_time);


        cdb_cd(rsock, "../../condition-clause-container/packet-security-ipv4-condition/pkt-sec-ipv4-src");
	range_src_ip_num = cdb_num_instances(rsock, "range-ipv4-address");
        for (i = 0; i < range_src_ip_num; i++) {
		cdb_get_ipv4(rsock, &src_start_ip_addr,"range-ipv4-address[%d]/start-ipv4-address", i);
		strncpy(str_src_start_ip_addr,inet_ntoa(src_start_ip_addr),sizeof(str_src_start_ip_addr));
	        printf("Src Start IP: %s\n", str_src_start_ip_addr);
		cdb_get_ipv4(rsock, &src_end_ip_addr,"range-ipv4-address[%d]/end-ipv4-address", i);
		strncpy(str_src_end_ip_addr,inet_ntoa(src_end_ip_addr),sizeof(str_src_end_ip_addr));
	        printf("Src End IP: %s\n", str_src_end_ip_addr);


	}


        cdb_cd(rsock, "../../../action-clause-container/advanced-action");
	cdb_get_list(rsock, &advanced_action, &advanced_action_num, "content-security-control");
	for (i = 0; i < advanced_action_num; i++) {
		printf("Advanced action: %d\n", CONFD_GET_IDENTITYREF(&advanced_action[i]).ns);
		printf("Advanced action: %d\n", CONFD_GET_IDENTITYREF(&advanced_action[i]).id);
		switch(CONFD_GET_IDENTITYREF(&advanced_action[i]).id) {
			case nsfintf_url_filtering:
				printf("Advanced action: %s\n", "url_filtering");
				break;
		}
	}
	
	

        //////////////////////////////////////////////
        //


//	printf("%s\n",str_src_start_ip_addr);
//	printf("%s\n",str_src_end_ip_addr);

	start_ptr = strtok(str_src_start_ip_addr,".");
	strncpy(common_ip_addr,start_ptr,sizeof(common_ip_addr));
	start_ptr = strtok(NULL,".");
	strncat(common_ip_addr,".",sizeof(common_ip_addr));
	strncat(common_ip_addr,start_ptr,sizeof(common_ip_addr));
	start_ptr = strtok(NULL,".");
	strncat(common_ip_addr,".",sizeof(common_ip_addr));
	strncat(common_ip_addr,start_ptr,sizeof(common_ip_addr));
	strncat(common_ip_addr,".",sizeof(common_ip_addr));

	start_ptr = strtok(NULL,".");

	strncpy(str_start_ptr,start_ptr,sizeof(str_start_ptr));


	end_ptr = strtok(str_src_end_ip_addr,".");
	end_ptr = strtok(NULL,".");
	end_ptr = strtok(NULL,".");
	end_ptr = strtok(NULL,".");

	strncpy(str_end_ptr,end_ptr,sizeof(str_end_ptr));

	int_start_ptr = atoi(str_start_ptr);
	int_end_ptr = atoi(str_end_ptr);

//	printf("%s\n",common_ip_addr);
//	printf("%d\n",int_start_ptr);
//	printf("%d\n",int_end_ptr);

	strncpy(temp_rule,"pass udp [", sizeof(temp_rule));
	
	for(i = int_start_ptr; i < int_end_ptr; i++) {
		strncat(temp_rule,common_ip_addr, sizeof(temp_rule));
		sprintf(temp_str, "%d", i);
		strncat(temp_rule,temp_str, sizeof(temp_rule));
		strncat(temp_rule,",", sizeof(temp_rule));
	}

	strncat(temp_rule,common_ip_addr, sizeof(temp_rule));
	sprintf(temp_str, "%d", int_end_ptr);
	strncat(temp_rule,temp_str, sizeof(temp_rule));
	strncat(temp_rule,"] any -> any", sizeof(temp_rule));



/*        for(i = 0; i < (range_src_ip_num); i++) {
		for(j = 0;strlen
              strncat(temp_rule, inet_ntoa(src_ip_list[i]), sizeof(temp_rule));
              strncat(temp_rule, ",", sizeof(temp_rule));
        }
        strncat(temp_rule, inet_ntoa(CONFD_GET_IPV4(&src_ip_list[i])), sizeof(temp_rule));

        strncat(temp_rule, "] any -> any", sizeof(temp_rule));
*/


/*      for(i = 0; i < (dest_ip_num -1); i++) {
                strncat(temp_rule, inet_ntoa(CONFD_GET_IPV4(&dest_ip_list[i])), sizeof(temp_rule));
                strncat(temp_rule, ",", sizeof(temp_rule));
        }
        strncat(temp_rule, inet_ntoa(CONFD_GET_IPV4(&dest_ip_list[i])), sizeof(temp_rule));
        printf("Dest IP: %s\n", inet_ntoa(CONFD_GET_IPV4(&dest_ip_list[i])));*/

        strncat(temp_rule, " any (msg:\"", sizeof(temp_rule));
        strncat(temp_rule, rule_name, sizeof(temp_rule));
        strncat(temp_rule, "\"; sid:", sizeof(temp_rule));
        sprintf(temp_itoa, "%d", rule_id);
        strncat(temp_rule, temp_itoa, sizeof(temp_rule));
        strncat(temp_rule, "; rev:1;)\n", sizeof(temp_rule));

        fputs(temp_rule,fp);

        rule_id++;

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
    cdb_set_namespace(rsock, nsfintf__ns);

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

    if ((status = cdb_subscribe(subsock, 3, nsfintf__ns, &spoint, "/i2nsf-security-policy"))
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

