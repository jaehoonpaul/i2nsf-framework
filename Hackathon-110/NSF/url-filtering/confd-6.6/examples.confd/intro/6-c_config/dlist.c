#include "dlist.h"
#include <stdlib.h>


Dlist *new_dlist()
{
  Dlist *d;

  d = (Dlist*) malloc (sizeof(struct dlist));
  d->flink = d;
  d->blink = d;
  return d;
}

/* Inserts before a given node */
void dl_insert_b(Dlist *node, void* v)
{
  Dlist *newnode;

  newnode = (Dlist*) malloc (sizeof(struct dlist));
  newnode->val = v;

  newnode->flink = node;
  newnode->blink = node->blink;
  newnode->flink->blink = newnode;
  newnode->blink->flink = newnode;
}

/* Inserts after a given node */
void dl_insert_a(Dlist *n, void* val)
{
  dl_insert_b(n->flink, val);
}

/* Inserts at the end of the list */
void dl_append(Dlist *l, void* val)
{
  dl_insert_b(l, val);
}

 /* Inserts at the beginning of the list */
void dl_prepend(Dlist *l, void* val)
{
  dl_insert_b(l->flink, val);
}

/* Deletes an arbitrary iterm */
void dl_delete_node(Dlist *node)
{
  node->flink->blink = node->blink;
  node->blink->flink = node->flink;
  free(node);
}

int dl_empty(Dlist *l)
{
  return (l->flink == l);
}

void free_dlist(Dlist *l)
{
  while (!dl_empty(l)) {
      dl_delete_node(dl_first(l));
  }
  free(l);
}

void* dl_val(Dlist *l)
{
  return l->val;
}

void* dl_find(Dlist* list, void* (f)(void*,void*), void* arg)
{
    Dlist *ptr;
    for (ptr = list->flink; ptr != list; ptr = ptr->flink) {
        if (f(ptr->val, arg))
            return ptr;
    }
    return NULL;
}
