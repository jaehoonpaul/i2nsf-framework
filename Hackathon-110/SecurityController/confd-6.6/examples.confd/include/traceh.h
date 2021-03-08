/****************************************************************
 * traceh.h
 * Simple C/C++ logging framework.
 * Framework can be used in ConfD C/C++ applications or generally.
 *
 * Copyright (c) 2017 Tail-F Systems
 * ver 1.1.0
 *
 * Permission to use this code as a starting point hereby granted
 * This is ConfD Sample Code
 *
 ****************************************************************/

#ifndef __TRACEH_H__
#define __TRACEH_H__

/**********************************************************
 * Supported compile time trace levels are:
 *  T_LOG_WARN
 *  T_LOG_INFO
 *  T_LOG_DEBUG
 *  T_LOG_TRACE
 *
 *  The compile time setup for logging levels
 *  is controlled by preprocessor definition T_LOG_<level>
 *  (e.g. `-DT_LOG_DEBUG`).
 *
 * To use build-in functions,
 * #define _TRACE_DECLARE
 * before including this header file (exactly in only one source file)
 * E.g.:
 *
 * #define _TRACE_DECLARE
 * #include <traceh.h>
 *
 * Update CFLAGS in Makefile to add include path and logging level.
 * E.g.:
 *
 * CFLAGS += -I$(CONFD_DIR)/examples.confd/include -DT_LOG_TRACE
 **********************************************************/

#include <stdio.h>
#include <sys/time.h>
#include <pthread.h>
#include <stdarg.h>
#include <syslog.h>

/* Additional trace levels to ones defined in syslog.h */
#define LOG_OFF     -1
#define LOG_TRACE   10

/* Turn on all the trace levels "under" the defined one */
#ifdef T_LOG_TRACE
#  define T_LOG_DEBUG
#  define T_LOG_INFO
#  define T_LOG_WARN
#elif defined T_LOG_DEBUG
#  define T_LOG_INFO
#  define T_LOG_WARN
#elif defined T_LOG_INFO
#  define T_LOG_WARN
#endif

enum time_format {
    simple, // seconds.nanoseconds
    formatted, // day-month-year::hour:minute:seconds.miliseconds microseconds
};

/**********************************************************
 * Interface functions to traceh.h
 **********************************************************/
extern void init_syslog_trace(const char *ident, int option, int facility);
extern void init_trace_streams(FILE* strout, FILE* strerr);
extern void set_trace_log_level(int level);
extern void set_trace_stream_level(int level);
extern void set_trace_syslog_level(int level);
extern void set_trace_time_format(enum time_format format);
extern int get_trace_log_level(void);
extern int get_trace_stream_level(void);
extern int get_trace_syslog_level(void);
extern int get_trace_log_compiled_level(void);
extern enum time_format get_trace_time_format(void);
extern int is_syslog_initialized(void);
extern int are_streams_initialized(void);
extern const char *get_trace_level_name(const int level);

extern void log_error(int level, const char *file, int line,
        const char *format, ...);
extern void log_stdout(int level, const char *file, int line,
        const char *prefix, const char *function, const char *format, ...);

/**********************************************************
 * Logging macros to use in the code
 **********************************************************/
// FATAL
#define FATAL(format, ...)\
    log_error(LOG_CRIT, __FILE__, __LINE__, format, ##__VA_ARGS__)

// WARN
#if defined(T_LOG_WARN)
#define WARN(format, ...)\
    log_error(LOG_WARNING, __FILE__, __LINE__, format, ##__VA_ARGS__)
#else
#define WARN(format, ...)   do {} while(0)
#endif

// INFO
#if defined(T_LOG_INFO)
# define INFO(format, ...)\
    log_stdout(LOG_INFO, __FILE__, __LINE__, NULL, NULL, format,\
     ##__VA_ARGS__)
# define INFO_ENTER(format, ...)\
    log_stdout(LOG_INFO, __FILE__, __LINE__, "==> ",\
            __FUNCTION__, format, ##__VA_ARGS__)
# define INFO_EXIT(format, ...)\
    log_stdout(LOG_INFO, __FILE__, __LINE__, "<== ",\
            __FUNCTION__, format, ##__VA_ARGS__)
#else
# define INFO(format, ...)          do {} while(0)
# define INFO_ENTER(format, ...)    do {} while(0)
# define INFO_EXIT(format, ...)     do {} while(0)
#endif

// DEBUG
#if defined(T_LOG_DEBUG)
# define DEBUG(format, ...)\
    log_stdout(LOG_DEBUG, __FILE__, __LINE__, NULL, NULL, format,\
     ##__VA_ARGS__)
# define DEBUG_ENTER(format, ...)\
    log_stdout(LOG_DEBUG, __FILE__, __LINE__, "==> ",\
            __FUNCTION__, format, ##__VA_ARGS__)
# define DEBUG_EXIT(format, ...)\
    log_stdout(LOG_DEBUG, __FILE__, __LINE__, "<== ",\
            __FUNCTION__, format, ##__VA_ARGS__)
#else
# define DEBUG(format, ...)         do {} while(0)
# define DEBUG_ENTER(format, ...)   do {} while(0)
# define DEBUG_EXIT(format, ...)    do {} while(0)
#endif

// TRACE
#if defined(T_LOG_TRACE)
# define TRACE(format, ...)\
    log_stdout(LOG_TRACE, __FILE__, __LINE__, NULL, NULL, format,\
     ##__VA_ARGS__)
# define TRACE_ENTER(format, ...)\
    log_stdout(LOG_TRACE, __FILE__, __LINE__, "==> ",\
            __FUNCTION__, format, ##__VA_ARGS__)
# define TRACE_EXIT(format, ...)\
    log_stdout(LOG_TRACE, __FILE__, __LINE__, "<== ",\
            __FUNCTION__, format, ##__VA_ARGS__)
# define _TRACE_BEGIN(format, ...)\
    log_stdout(LOG_TRACE, __FILE__, __LINE__, "--> ",\
            NULL, format, ##__VA_ARGS__)
# define _TRACE_END(format, ...)\
    log_stdout(LOG_TRACE, __FILE__, __LINE__, "<-- ",\
            NULL, format, ##__VA_ARGS__)
#else
# define TRACE(format, ...)         do {} while(0)
# define TRACE_ENTER(format, ...)   do {} while(0)
# define TRACE_EXIT(format, ...)    do {} while(0)
# define _TRACE_BEGIN(format, ...)  do {} while(0)
# define _TRACE_END(format, ...)    do {} while(0)
#endif

/**************************************************************************
 * Declarations - enable this part (#define _TRACE_DECLARE before including
 * this file) in exactly one source file.
 **************************************************************************/

#ifdef _TRACE_DECLARE
/**********************************************************
 * Global variable declarations
 **********************************************************/
#ifdef T_LOG_TRACE
static int _t_log_level = LOG_TRACE;
static int _t_stream_level = LOG_TRACE;
static int _t_syslog_level = LOG_DEBUG; // LOG_DEBUG is max. prio. for syslog
static const int _t_log_compiled_level = LOG_TRACE;
#elif defined T_LOG_DEBUG
static int _t_log_level = LOG_DEBUG;
static int _t_stream_level = LOG_DEBUG;
static int _t_syslog_level = LOG_DEBUG;
static const int _t_log_compiled_level = LOG_DEBUG;
#elif defined T_LOG_INFO
static int _t_log_level = LOG_INFO;
static int _t_stream_level = LOG_INFO;
static int _t_syslog_level = LOG_INFO;
static const int _t_log_compiled_level = LOG_INFO;
#elif defined T_LOG_WARN
static int _t_log_level = LOG_WARNING;
static int _t_stream_level = LOG_WARNING;
static int _t_syslog_level = LOG_WARNING;
static const int _t_log_compiled_level = LOG_WARNING;
#else
static int _t_log_level = LOG_ERR;
static int _t_stream_level = LOG_ERR;
static int _t_syslog_level = LOG_ERR;
static const int _t_log_compiled_level = LOG_ERR;
#endif

static int t_syslog_initialized = 0;
static enum time_format _t_time_format = formatted;
static FILE* _t_strout = NULL;
static FILE* _t_strerr = NULL;

static pthread_mutex_t tracelog_mutex = PTHREAD_MUTEX_INITIALIZER;

/**********************************************************
 * Function declarations
 **********************************************************/
const char *get_trace_level_name(const int level)
{
    switch (level) {
        case LOG_OFF: return "OFF"; // -1
        case LOG_EMERG: return "EMERG";// 0
        case LOG_ALERT: return "ALERT";// 1
        case LOG_CRIT: return "CRIT";// 2
        case LOG_ERR: return "ERR";// 3
        case LOG_WARNING: return "WARNING";// 4
        case LOG_NOTICE: return "NOTICE";// 5
        case LOG_INFO: return "INFO";// 6
        case LOG_DEBUG: return "DEBUG";// 7
        case LOG_TRACE: return "TRACE";// 10
    }

    return "UNKNOWN";
}

void init_syslog_trace(const char *ident, int option, int facility)
{
    INFO("Initializing syslog");
    openlog(ident, option, facility);
    t_syslog_initialized = 1;
}

void init_trace_streams(FILE* strout, FILE* strerr)
{
    INFO("Initializing trace streams: strout=%p, strerr=%p,"
            " fileno(strout)=%d, fileno(strerr)=%d", strout, strerr,
            strout ? fileno(strout) : -1,
            strerr ? fileno(strerr): -1);

    _t_strout = strout;
    _t_strerr = strerr;
}

void set_trace_log_level(int level)
{
    INFO("Setting log level to: %s", get_trace_level_name(level));
    _t_log_level = level;
}

void set_trace_stream_level(int level)
{
    INFO("Setting stream level to: %s", get_trace_level_name(level));
    _t_stream_level = level;
}

void set_trace_syslog_level(int level)
{
    INFO("Setting syslog level to: %s", get_trace_level_name(level));
    _t_syslog_level = level;
}

void set_trace_time_format(enum time_format format)
{
    INFO("Setting time format to: %i",format);
    _t_time_format = format;
}

int get_trace_log_level(void)
{
    return _t_log_level;
}

int get_trace_syslog_level(void)
{
    return _t_syslog_level;
}

int get_trace_stream_level(void)
{
    return _t_stream_level;
}

int get_trace_log_compiled_level(void)
{
    return _t_log_compiled_level;
}

enum time_format get_trace_time_format(void)
{
    return _t_time_format;
}

int is_syslog_initialized(void) {
    return t_syslog_initialized;
}

int are_streams_initialized(void) {
    return _t_strerr != NULL && _t_strout != NULL;
}

void fill_time_buf(char * time_buf, const int time_buf_size,
        const struct timeval* tv)
{
    if (_t_time_format == simple) {
        snprintf(time_buf, time_buf_size ,"%lu.%06lu",
                (unsigned long)tv->tv_sec,
                (unsigned long)tv->tv_usec);
    } else {
        struct tm* tm_info = localtime(&(tv->tv_sec));
        char tmp_buffer[26];
        strftime(tmp_buffer, sizeof(tmp_buffer), "%d-%b-%Y::%H:%M:%S", tm_info);
        snprintf(time_buf, time_buf_size,"%s.%03lu %03luus", tmp_buffer,
                ((unsigned long)tv->tv_usec)/1000,
                ((unsigned long)tv->tv_usec)%1000);
    }
}

void log_error(int level, const char *file, int line, const char *format, ...)
{
    pthread_mutex_lock(&tracelog_mutex);
    if (level <= _t_log_level
            || (t_syslog_initialized && level <= LOG_DEBUG &&
                    level <= _t_syslog_level)
            || (_t_strerr != NULL && level <= _t_stream_level)) {
        va_list ap;
        struct timeval tv;
        char buf[1024];
        char time_buf[32];
        gettimeofday(&tv, NULL);
        fill_time_buf(time_buf, sizeof(time_buf), &tv);
        snprintf(buf, sizeof(buf),
                "%s [%lx] %s %s:%d: \033[91m %s\033[0m\n", time_buf,
                (long)pthread_self(), get_trace_level_name(level),
                file, line, format);

        // Log to standard error I/O stream
        if (level <= _t_log_level) {
            va_start(ap, format);
            vfprintf(stderr, buf, ap);
            va_end(ap);
        }

        // Log to error I/O stream
        if (_t_strerr != NULL && level <= _t_stream_level) {
            va_start(ap, format);
            vfprintf(_t_strerr, buf, ap);
            fflush(_t_strerr);
            va_end(ap);
        }

        // Log to syslog
        // LOG_DEBUG is the max. priority value for syslog
        if (t_syslog_initialized && level <= LOG_DEBUG &&
                level <= _t_syslog_level) {
            va_start(ap, format);
            vsyslog(level, buf, ap);
            va_end(ap);
        }
    }
    pthread_mutex_unlock(&tracelog_mutex);
}

void log_stdout(int level, const char *file, int line,
        const char *prefix, const char *function, const char *format, ...)
{
    pthread_mutex_lock(&tracelog_mutex);
    if (level <= _t_log_level
            || (t_syslog_initialized && level <= LOG_DEBUG &&
                    level <= _t_syslog_level)
            || (_t_strout != NULL && level <= _t_stream_level)) {
        va_list ap;
        struct timeval tv;
        char buf[1024];
        char time_buf[32];
        const char *empty_string = "";
        const char *space_string = " ";
        const char *function_suffix = space_string;

        if (!prefix) {
            prefix = empty_string;
        }
        if (!function) {
            function = empty_string;
            function_suffix = empty_string;
        }
        gettimeofday(&tv, NULL);
        fill_time_buf(time_buf, sizeof(time_buf), &tv);
        snprintf(buf, sizeof(buf), "%s [%lx] %s %s:%d: %s%s%s%s\n",
                time_buf, (long)pthread_self(),
                get_trace_level_name(level),
                file, line, prefix, function, function_suffix, format);

        // Log to standard I/O stream
        if (level <= _t_log_level) {
            va_start(ap, format);
            // if you are using stdout, it is advisable to fflush after write...
            vfprintf(stdout, buf, ap);
            fflush(stdout);
            va_end(ap);
        }

        // Log to I/O stream
        if (_t_strout != NULL && level <= _t_stream_level) {
            va_start(ap, format);
            vfprintf(_t_strout, buf, ap);
            fflush(_t_strout);
            va_end(ap);
        }

        // Log to syslog
        // LOG_DEBUG is the max. priority value for syslog
        if (t_syslog_initialized &&
                level <= LOG_DEBUG && level <= _t_syslog_level) {
            va_start(ap, format);
            vsyslog(level, buf, ap);
            va_end(ap);
        }
    }
    pthread_mutex_unlock(&tracelog_mutex);
}
#endif  /* _TRACE_DECLARE */

// End of logging support

#endif  /* __TRACEH_H__ */
