
#ifndef DLIST_H
#define DLIST_H


typedef struct dlist {
  struct dlist *flink;
  struct dlist *blink;
  void* val;
} Dlist;

extern Dlist *new_dlist();
extern void free_dlist(Dlist*);

extern void dl_append(Dlist*, void*);
extern void dl_prepend(Dlist*, void*);
extern void dl_insert_b(Dlist*, void*);
extern void dl_insert_a(Dlist*, void*);

extern void dl_delete_node(Dlist*);
extern int dl_empty(Dlist*);

extern void* dl_val(Dlist*);

extern void *dl_find(Dlist*, void* (f)(void*,void*), void*);
#define dl_first(d) ((d)->flink)
#define dl_next(d) ((d)->flink)
#define dl_last(d) ((d)->blink)
#define dl_prev(d) ((d)->blink)
#define dl_nil(d) (d)

#define dl_traverse(ptr, list) \
  for (ptr = list->flink; ptr != list; ptr = ptr->flink)
#define dl_rtraverse(ptr, list) \
  for (ptr = list->blink; ptr != list; ptr = ptr->blink)


#endif


