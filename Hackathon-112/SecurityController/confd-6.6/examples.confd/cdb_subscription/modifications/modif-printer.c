#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include <confd.h>

#define BUFF_LEN 65536
#define INDENT_SIZE 4
#define INDENT_STR ""

struct pr_doc {
    size_t alloc_len;
    int len;
    char *data;
};


static void doc_init(struct pr_doc *document)
{
    document->alloc_len = BUFF_LEN;
    document->len = 0;
    document->data = malloc(document->alloc_len);
    memset(document->data, 0x00, document->alloc_len);
}


static int doc_append(struct pr_doc *document, char *str)
{
    size_t str_len = strnlen(str, BUFF_LEN);
    size_t remaining_len = (document->alloc_len - document->len);

    if (str_len > remaining_len) {
        document->data = realloc(document->data,
                            document->alloc_len + BUFF_LEN);
    }

    strncpy(document->data + document->len, str, str_len);
    document->len += str_len;

    return str_len;
}


static int write_value(struct pr_doc *document, confd_tag_value_t *value,
        int *indent)
{
    char *tag_str = confd_xmltag2str(value->tag.ns, value->tag.tag);

    char buff[BUFF_LEN];
    char value_buff[BUFF_LEN];

    switch (value->v.type) {

        // start a container/list entry creation/modification
        case C_XMLBEGIN:
            snprintf(buff, sizeof(buff), "%*s<%s>\n", *indent, INDENT_STR,
                    tag_str);
            *indent += INDENT_SIZE;
            break;

        // exit from a processing of container/list entry creation/modification
        case C_XMLEND:
            *indent -= INDENT_SIZE;
            snprintf(buff, sizeof(buff), "%*s</%s>\n", *indent, INDENT_STR,
                    tag_str);
            break;

        // deletion of a leaf
        case C_NOEXISTS:
            snprintf(buff, sizeof(buff), "%*s<%s operation=\"delete\">\n",
                    *indent, INDENT_STR, tag_str);
            break;

        // deletion of a list entry / container
        case C_XMLBEGINDEL:
            snprintf(buff, sizeof(buff), "%*s<%s operation=\"delete\">\n",
                    *indent, INDENT_STR, tag_str);
            *indent += INDENT_SIZE;
            break;

        // type empty leaf creation
        case C_XMLTAG:
            snprintf(buff, sizeof(buff), "%*s<%s/>\n", *indent, INDENT_STR,
                    tag_str);
            break;

        // regular leaf creation/modification
        default:
            confd_pp_value(value_buff, sizeof(value_buff), &value->v);
            snprintf(buff, sizeof(buff), "%*s<%s>%s</%s>\n", *indent,
                    INDENT_STR, tag_str, value_buff, tag_str);
    }

    int chars_written = doc_append(document, buff);
    return chars_written;
}


char * pr_write_value_array(
    confd_tag_value_t *values,
    int values_cnt
) {
    struct pr_doc doc;
    doc_init(&doc);

    int indent = 0;

    int i;
    for (i = 0; i < values_cnt; i++) {
        write_value(&doc, &values[i], &indent);
    }

    return doc.data;
}