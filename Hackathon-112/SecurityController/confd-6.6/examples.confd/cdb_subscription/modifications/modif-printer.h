#include <confd.h>

// Allocate a string containing printout of the tagged value array.
// Output follows a NETCONF edit-config message format without any headers.
char * pr_write_value_array(
    confd_tag_value_t *values,
    int values_cnt
);