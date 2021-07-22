#include<stdio.h>
#include<json-c/json.h>
#include<stdlib.h>
#include<time.h>
#include<string.h>
#include<stdarg.h>
#include<stdlib.h>

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

int main(int argc, char **argv) {
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

	status = getJson("system_status");
	cpu_usage = getJson("cpu_usage");
	memory_usage = getJson("memory_usage");
	disk_usage = getJson("disk_usage");
	disk_left = getJson("disk_left");
	in_traffic_speed = getJson("in_traffic_speed");
	out_traffic_speed = getJson("out_traffic_speed");



	printf("Status: %s\n", json_object_get_string(status));
        printf("CPU Usage: %.2f\n", json_object_get_double(cpu_usage));
        printf("Mem Usage: %.2f\n", json_object_get_double(memory_usage));
        printf("Disk Usage: %d\n", json_object_get_int(disk_usage));
        printf("Disk Left: %d\n", json_object_get_int(disk_left));
        printf("In Traffic Speed: %d\n", json_object_get_int(in_traffic_speed));
        printf("Out Traffic Speed: %d\n", json_object_get_int(out_traffic_speed));


}
