#include <pthread.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <netinet/udp.h>
#include <netinet/in.h>
#include "ietf-i2nsf-policy-rule-for-nsf.h"


int main(int argc, char *argv[]) {
    if(argc != 1) { 
        printf("Usage: ./firewall \n");
        exit(-1);
    }

	start_confd();


    return 0;
}
