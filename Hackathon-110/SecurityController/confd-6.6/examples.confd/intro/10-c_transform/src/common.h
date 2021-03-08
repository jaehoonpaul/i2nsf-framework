/*********************************************************************
 * ConfD Transformation callpoint example
 *
 * This is ConfD Sample Code.
 *
 * (C) 2017 Tail-f Systems
 * Permission to use this code as a starting point hereby granted
 *
 * See the README file for more information
 ********************************************************************/

#ifndef COMMON_H
#define COMMON_H

#define TRANSFORM_BUFF_LEN 8192

// helper procedure to terminate daemon on unwanted errors
void OK(int rval);

// helper trace for ConfD value
void print_val(const char *prefix, const confd_value_t *val);

// helper trace for ConfD keypath
void print_path(const char *prefix, const confd_hkeypath_t *kp);

// the "global" variables passed around the daemon execution code
struct global_vars {
    int ctlsock;
    int workersock;
    int maapi_socket;
};

// and the "instance" placeholder
extern struct global_vars glob;

#endif