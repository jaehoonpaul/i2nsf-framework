extern char *optarg;
extern int optind;
extern int optopt;
extern int opterr;

#define no_argument        0
#define required_argument  1
#define optional_argument  2
struct option {
    char *name;
    int has_arg;
    int *flag;
    int val;
};
extern int
getopt_long(int argc, char * const *argv, const char *optstring,
            const struct option *longopts, int *longindex);
