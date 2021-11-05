#include <sys/poll.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <sys/poll.h>
#include <string.h>

#include <stdio.h>
#include <expat.h>

#define BUFSIZE 8192

/* these are the states in our simple state machine.  the state machine
   is driven by the XML callbacks start_tag and data */
enum state {
    ST_INIT,
    ST_READ_SUBSCRIBE,
    ST_READ_EVENTS,
    ST_DONE};

/* global state variable */
enum state state;

/* global events variable */
int events;

static char zero_buf[1];

/* write a reply to the manager */
static void rpc_ok()
{
    char buf[BUFSIZE];
    sprintf(buf, "<ok/>\n");
    write(1, buf, strlen(buf));
    /* write message separator to ConfD */
    write(1, zero_buf, 1);
}

static void send_event(char *str, int arg)
{
    char buf[BUFSIZE];
    sprintf(buf,
            "<event xmlns=\"http://example.com/event\">\n"
            "  <message>%s%d</message>\n"
            "</event>\n", str, arg);
    write(1, buf, strlen(buf));
    /* write message separator to ConfD */
    write(1, zero_buf, 1);
}

/* write an error to the manager and terminate properly */
static void rpc_error(char *str, const char *xtra)
{
    char buf[BUFSIZE];
    sprintf(buf,
            "<rpc-error>\n"
            "  <error-type>application</error-type>\n"
            "  <error-tag>operation-failed</error-tag>\n"
            "  <error-severity>error</error-severity>\n"
            "  <error-message xml:lang='en'>\n"
            "    %s%s\n"
            "  </error-message>\n"
            "</rpc-error>\n",
            str, xtra);
    write(1, buf, strlen(buf));
    exit(0);
}

static void XMLCALL start_tag(void *data, const char *tag, const char **attr)
{
     switch (state) {
     case ST_INIT:
         if (strcmp(tag, "subscribe") == 0) {
             state = ST_READ_SUBSCRIBE;
         }
         else
             rpc_error("bad rpc from ConfD: ", tag);
         break;
     case ST_READ_SUBSCRIBE:
         if (strcmp(tag, "events") == 0) {
             state = ST_READ_EVENTS;
         }
         else
             rpc_error("unknown operation: ", tag);
         break;
     default:
         rpc_error("unknown element: ", tag);
     }
}

#define IS_WS(x) (((x) == ' ' || (x) == '\t' || (x) == '\n' || (x) == '\r'))

static void strip(const char *src, char *src_end, char *dst)
{
    char *p, *e, *r;

    p = (char *)src;
    e = src_end;
    r = dst;
    dst[0] = '\0';
    /* skip leading whitespace */
    while (p != src_end && IS_WS(*p)) {
        p++;
    }
    if (p == src_end)
        return;
    /* skip trailing whitespace */
    while (e > p && IS_WS(*e)) {
        e--;
    }
    e++;
    /* copy rest */
    do {
        *r = *p;
        r++;
        p++;
    } while (p != e);
    *r = '\0';
}

/* this callback is invoked by expat whenever we get some data, including
   whitespace only data. */
static void XMLCALL data(void *data, const XML_Char *s, int len)
{
    char content[BUFSIZE];

    /* accept empty data */
    strip(s, ((char *)s)+len, content);
    if (content[0] == '\0')
        return;

    switch (state) {
    case ST_READ_EVENTS:
        events = atoi(content);
        if (events < 0) {
            rpc_error("number of events must be >= 0: ", s);
        }
        state = ST_DONE;
        break;
    default:
        rpc_error("unknown data: ", s);
    }
}

int main(int argc, char *argv[]) {
    int done;
    int len;
    char buf[BUFSIZE];
    int res;
    int i;

    zero_buf[0] = 0;

    XML_Parser p;

    if ((p = XML_ParserCreate(NULL)) == NULL) {
        fprintf(stderr, "Couldn't allocate memory for parser\n");
        exit(1);
    }

    XML_SetElementHandler(p, start_tag, NULL);
    XML_SetCharacterDataHandler(p, data);

    /* read the complete RPC from stdin; ConfD signals end-of-input
       by closing stdin. */
    for (;;) {
        len = fread(buf, 1, BUFSIZE, stdin);

        if (ferror(stdin)) {
            fprintf(stderr, "Read error\n");
            exit(1);
        }

        done = feof(stdin);

        state = ST_INIT;
        if (XML_Parse(p, buf, len, done) == XML_STATUS_ERROR) {
            fprintf(stderr,
                    "Parse error at line %d u:\n%s\n",
                    (int)XML_GetCurrentLineNumber(p),
                    XML_ErrorString(XML_GetErrorCode(p)));
            exit(1);
        }

        if (done)
            break;
    }

    /* stdin is closed, thus we have read all data.  make sure that we're
       in correct state. */

    if (state != ST_DONE)
        rpc_error("incomplete message", "");

    /* reply to ConfD and then enter batch mode */
    rpc_ok(res);

    /* trivial batch mode - send N simple strings */
    for (i = 1; i < events; i++) {
        sleep(4);
        send_event("this is event number: ", i);
    }
    sleep(4);
    send_event("this is the last event, number: ", i);

    return 0;
}
