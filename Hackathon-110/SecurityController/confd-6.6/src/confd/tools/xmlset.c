#include <stdio.h>
#include <string.h>
#include <expat.h>

/*
This disables the webui:
$ ./xmlset C false confdConfig webui enabled < ../../../system/confd.conf

This removes the encryptedStrings container:
$ ./xmlset R confdConfig encryptedStrings < ../../../system/confd.conf
*/

#ifdef XML_LARGE_SIZE
#if defined(XML_USE_MSC_EXTENSIONS) && _MSC_VER < 1400
#define XML_FMT_INT_MOD "I64"
#else
#define XML_FMT_INT_MOD "ll"
#endif
#else
#define XML_FMT_INT_MOD "l"
#endif

#define BUFSIZE 8192

char BUF[BUFSIZE];

/* A stack of tags. */
char TAG_STACK[16][32];
/* The current depth of the tag stack. */
int N = -1;

/* The tagpath to be removed. */
char REMOVE[16][32];
/* The length of the above tagpath. */
int REMOVE_LEN = 0;

/* The tagpath to be changed. */
char CHANGE[16][32];
/* The length of the above tagpath */
int CHANGE_LEN = 0;
/* The new value to change into. */
char CHANGE_VAL[2048];

/* Is this a leaf or container element. */
int IN_CONTAINER = 0;

int skip() {
     int i;

     if (REMOVE_LEN == 0)
          return 0;

     for (i = 0; i < REMOVE_LEN; i ++)
          if (i > N)
               return 0;
          else
               if (strcmp(REMOVE[i], TAG_STACK[i]) != 0)
                    return 0;

     return 1;
}

int change() {
     int i;

     if (CHANGE_LEN == 0)
          return 0;

     for (i = 0; i < CHANGE_LEN; i++)
          if (i > N)
               return 0;
          else
               if (strcmp(CHANGE[i], TAG_STACK[i]) != 0)
                    return 0;

     return 1;
}

static void XMLCALL start(void *data, const char *tag, const char **attr) {
     int i;

     strcpy((char *)TAG_STACK[++N], tag);

     if (!skip()) {
          printf("<%s", tag);

          for (i = 0; attr[i]; i += 2)
               printf(" %s='%s'", attr[i], attr[i+1]);

          printf(">");
     }

     IN_CONTAINER = 1;
}

static void XMLCALL end(void *data, const char *tag) {
     int i;

     if (!skip())
          printf("</%s>", TAG_STACK[N]);

     --N;
     IN_CONTAINER = 0;
}

static void XMLCALL data(void *data, const XML_Char *s, int len) {
     if (!skip())
          if (change() && IN_CONTAINER) {
               printf("%s", CHANGE_VAL);
               IN_CONTAINER = 0;
          } else
               printf("%.*s", len, s);
}

static void XMLCALL comment(void *data, const XML_Char *s) {
     if (!skip())
          printf("<!--%s-->", s);
}

int extract_args(int argc, char *argv[]) {
     int mode;
     int i;

     if (argc < 2)
         return 0;

     switch (*argv[1]) {
     case 'R':
         mode = 0;
         for (i = 0; i < argc-2; i++) {
             ++REMOVE_LEN;
             strcpy(REMOVE[i], argv[i+2]);
         }
         return 1;
     case 'C':
         if (argc < 3)
             return 0;
         mode = 1;
         strcpy(CHANGE_VAL, argv[2]);
         for (i = 0; i < argc-3; i++) {
             ++CHANGE_LEN;
             strcpy(CHANGE[i], argv[i+3]);
         }
         return 1;
     }

     return 0;
}

main(int argc, char *argv[]) {
     int done;
     int len;

     XML_Parser p;

     if ((p = XML_ParserCreate(NULL)) == NULL) {
          fprintf(stderr, "Couldn't allocate memory for parser\n");
          exit(2);
     }

     XML_SetElementHandler(p, start, end);
     XML_SetCharacterDataHandler(p, data);
     XML_SetCommentHandler(p, comment);

     if (!extract_args(argc, argv)) {
          fprintf(stderr, "Usage: %s R Tag ... < XmlFile\n", argv[0]);
          fprintf(stderr, "Usage: %s C NewValue Tag ... < XmlFile\n", argv[0]);
          exit(1);
     }

     for (;;) {
          len = fread(BUF, 1, BUFSIZE, stdin);

          if (ferror(stdin)) {
               fprintf(stderr, "Read error\n");
               exit(2);
          }

          done = feof(stdin);

          if (XML_Parse(p, BUF, len, done) == XML_STATUS_ERROR) {
               fprintf(stderr,
                       "Parse error at line %" XML_FMT_INT_MOD "u:\n%s\n",
                       XML_GetCurrentLineNumber(p),
                       XML_ErrorString(XML_GetErrorCode(p)));
               exit(2);
          }

          if (done)
               break;
     }

     return 0;
}
