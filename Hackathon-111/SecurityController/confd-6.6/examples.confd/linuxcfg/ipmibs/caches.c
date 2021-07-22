#include <time.h>
#include <malloc.h>
#include <stdlib.h>
#include <string.h>

#include <confd.h>
#include "linuxcfg_api.h"

#include "caches.h"

void check_update_table(dummy_c_table_t * c_table,
                        int update_interval)
{
    time_t now;
    time_t diff;
    time(&now);

    diff = difftime(now, c_table->timestamp);
    if (diff >= update_interval) {
        c_table->update(&c_table->table);
        c_table->timestamp = now;
    }

}

void check_reserve(dummy_list_t * list)
{
    int i;

    if (list->required >= list->allocated) {
        for (i = list->allocated == 0 ? INITIAL_LIST_SIZE : list->allocated;
             i <= list->required; i *= 2);
        list->entries = realloc(list->entries, i * list->entry_size);
        list->indices = realloc(list->indices, i * sizeof(int));
        if (list->entries == NULL || list->indices == NULL) {
            fail("insufficient memory");
        }
        memset(((char*)list->entries) + list->allocated * list->entry_size,
               0, (i - list->allocated) * list->entry_size);
        list->allocated = i;
    }
}

void check_realloc(dummy_list_t * list)
{
    int i, ix = list->required;

    check_reserve(list);
    if (ix >= list->size) {
        for (i = list->size; i <= ix; i++)
            list->indices[i] = i;
        list->size = ix + 1;
    }
}

static dummy_list_t *d_list;
static int compar(const void *p1, const void *p2)
{
    return d_list->cmp(DUMMY_IX(d_list, *(int *) p1),
                       DUMMY_IX(d_list, *(int *) p2));
}

void sort_list(dummy_list_t * list)
{
    d_list = list;
    qsort(list->indices, LIST_SIZE(list), sizeof(int), compar);
}

/**
 * Remove any duplicities in the list. Duplicity is detected with the
 * list's comparison function; the list needs to be ordered before
 * call to this function.
 */
void filter_list(dummy_list_t *list)
{
    unsigned gap, i, pi;

    for (gap = 0, pi = 0, i = 1; i < LIST_SIZE(list); i++) {
        if (list->cmp(DUMMY_LISTENTRY(list, i),
                      DUMMY_LISTENTRY(list, pi)) == 0) {
            // duplicity detected
            gap++;
        } else {
            if (gap > 0)
                list->indices[++pi] = list->indices[i];
        }
    }
    list->size -= gap;
}
