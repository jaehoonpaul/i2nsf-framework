#ifndef _CACHES_H
#define _CACHES_H

#define UPDATE_INTERVAL 3        // time (in sec) after which a table is
                                 // considered obsolete

#include <time.h>

#define CACHE_TABLE(table_type)                 \
    struct {                                    \
        time_t timestamp;                       \
        void (*update)(table_type *table);      \
        table_type table;                       \
    }

typedef CACHE_TABLE(void *) dummy_c_table_t;
void check_update_table(dummy_c_table_t * c_table,
                        int update_interval);

#define INIT_TABLE(c_table, update_handler) do {    \
        (c_table)->timestamp = 0;                    \
        (c_table)->update = update_handler;            \
    } while (0)

#define _GET_TABLE(c_table) (&((c_table)->table))
#define GET_TABLE(c_table) (check_update_table((dummy_c_table_t*)(c_table),\
                                               UPDATE_INTERVAL),           \
                            _GET_TABLE(c_table))
#define GET_TABLE_NOCACHE(c_table) (check_update_table(                  \
                                        (dummy_c_table_t*)(c_table), 0), \
                                    _GET_TABLE(c_table))


#define INITIAL_LIST_SIZE 4

#define ENTRY_COMPARATOR(entry_type, cmp) \
    int cmp(entry_type const *o1, entry_type const *o2)
#define ROW_LIST(entry_type)                    \
    struct {                                    \
        int size, allocated, required;          \
        int entry_size;                         \
        entry_type *entries;                    \
        int *indices;                           \
        ENTRY_COMPARATOR(entry_type, (*cmp));   \
    }

typedef ROW_LIST(void *) dummy_list_t;
typedef ENTRY_COMPARATOR(void *, dummy_comparator_t);
void check_reserve(dummy_list_t * list);
void check_realloc(dummy_list_t * list);
void sort_list(dummy_list_t * list);
void filter_list(dummy_list_t * list);

#define DUMMY_IX(list, i) \
    ((void**)((char*)(list)->entries + (list)->entry_size * (i)))
#define DUMMY_LISTENTRY(list, i) DUMMY_IX(list, (list)->indices[i])
#define LISTENTRY(list, i) (&(list)->entries[(list)->indices[i]])
  // use with caution! (no check for array bounds)

/**
 * Initialize the list, supply a comparator (i.e. a function accepting
 * two pointers to list entries returning int with the semantic
 * similar to strcmp).
 */
#define INIT_LIST(list, _cmp) ((list)->entries = NULL,                    \
                               (list)->indices = NULL,                    \
                               (list)->size = (list)->allocated = 0,    \
                               (list)->entry_size = sizeof(*(list)->entries), \
                               (list)->cmp = _cmp)
#define RESET_LIST(list) ((list)->size = 0)
/**
 * Initialize the table-list (i.e. the table which is actually a
 * list). See INIT_LIST for details about cmp.
 */
#define INIT_LIST_TABLE(c_table, update_handler, cmp) do { \
        INIT_TABLE(c_table, update_handler);               \
        INIT_LIST(&(c_table)->table, cmp);                   \
    } while(0)
#define LIST_SIZE(list) (list)->size
#define NEW_ENTRY(list) GET_ENTRY(list, (list)->size)
#define GET_ENTRY(list, index) ((list)->required = index,            \
                                check_realloc((dummy_list_t*)list),    \
                                LISTENTRY(list, (list)->required))
#define RESERVE_ENTRIES(list, count) ((list)->required = (list)->size + count, \
                                      check_reserve((dummy_list_t*) list))
#define FOR_ENTRIES(list, entry, i)                                        \
    for (i = 0;                                                            \
         (entry = i < LIST_SIZE(list) ? LISTENTRY(list, i) : NULL) != NULL;    \
         i++)
#define FREE_LIST(list) do {       \
        if ((list)->allocated) {   \
            free((list)->entries); \
            free((list)->indices); \
        } while(0)
#define SORT_LIST(list) sort_list((dummy_list_t*) (list))
#define FILTER_LIST(list) filter_list((dummy_list_t*) (list))

#endif // _CACHES_H
