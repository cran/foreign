/* libavl - manipulates AVL trees.
   Copyright (C) 1998-9, 2000 Free Software Foundation, Inc.
   Modified for R foreign library by Saikat DebRoy <saikat@stat.wisc.edu>.

   This program is free software; you can redistribute it and/or
   modify it under the terms of the GNU General Public License as
   published by the Free Software Foundation; either version 2 of the
   License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301
   USA.

   The author may be contacted at <pfaffben@pilot.msu.edu> on the
   Internet, or as Ben Pfaff, 12167 Airport Rd, DeWitt MI 48820, USA
   through more mundane means. */

/* This is file avl.c in libavl. */

#include <stdio.h>
#include <stdlib.h>
#include <R.h>
#include "avl.h"

#if !PSPP && !__GCC__
#define inline
#endif

#if !PSPP
#if __GNUC__ >= 2
#define unused __attribute__ ((unused))
#else
#define unused
#endif
#endif

static void Free_fn(void *x)
{
    Free(x);
}

/* Creates an AVL tree in POOL (which can be NULL).  POOL is owned by
   the caller, not by the AVL tree.  CMP is a order function for the
   data to be stored in the tree.  PARAM is arbitrary data that
   becomes an argument to the comparison function. */
avl_tree *
avl_create (MAYBE_POOL avl_comparison_func cmp, void *param)
{
  avl_tree *tree;

  if (!(cmp != NULL)) error("assert failed : cmp != NULL");
#if PSPP
  if (pool)
    tree = pool_alloc (pool, sizeof *tree);
  else
#endif
    tree = Calloc (1, avl_tree);

#if PSPP
  tree->pool = pool;
#endif
  tree->root.link[0] = NULL;
  tree->root.link[1] = NULL; 
  tree->cmp = cmp;
  tree->count = 0;
  tree->param = param;

  return tree;
}

/* Destroy tree TREE.  Function FREE_FUNC is called for every node in
   the tree as it is destroyed.  

   No effect if the tree has an pool owner and free_func is NULL.
   The caller owns the pool and must destroy it itself.

   Do not attempt to reuse the tree after it has been freed.  Create a
   new one.  */
void
avl_destroy (avl_tree *tree, avl_node_func free_func)
{
  if (!(tree != NULL)) error("assert failed : tree != NULL");
  
#if PSPP
  if (free_func || tree->pool == NULL)
#endif
    {
      /* Uses Knuth's Algorithm 2.3.1T as modified in exercise 13
	 (postorder traversal). */
      
      /* T1. */
      avl_node *an[AVL_MAX_HEIGHT];	/* Stack A: nodes. */
      char ab[AVL_MAX_HEIGHT];		/* Stack A: bits. */
      int ap = 0;			/* Stack A: height. */
      avl_node *p = tree->root.link[0];

      for (;;)
	{
	  /* T2. */
	  while (p != NULL)
	    {
	      /* T3. */
	      ab[ap] = 0;
	      an[ap++] = p;
	      p = p->link[0];
	    }

	  /* T4. */
	  for (;;)
	    {
	      if (ap == 0)
		goto done;

	      p = an[--ap];
	      if (ab[ap] == 0)
		{
		  ab[ap++] = 1;
		  p = p->link[1];
		  break;
		}
      
	      if (free_func)
		free_func (p->data, tree->param);
#if PSPP
	      if (tree->pool == NULL)
#endif
		Free (p);
	    }
	}
    }

 done:
#if PSPP
  if (tree->pool == NULL)
#endif
    Free (tree);
}

/* avl_destroy() with FREE_FUNC hardcoded as free(). */
void
avl_free (avl_tree *tree)
{
  avl_destroy (tree, (avl_node_func) Free_fn);
}

/* Return the number of nodes in TREE. */
int
avl_count (const avl_tree *tree)
{
  if (!(tree != NULL)) error("assert failed : tree != NULL");
  return tree->count;
}

/* Allocates room for a new avl_node in POOL, or using Calloc() if
   POOL is NULL. */
#if PSPP
static inline avl_node *
new_node (struct pool *pool)
{
  if (pool != NULL)
    return pool_alloc (pool, sizeof (avl_node));
  else
    return Calloc (1, avl_node);
}
#else
static inline avl_node *
new_node (void)
{
  return Calloc (1, avl_node);
}

#define new_node(POOL)				\
	new_node ()
#endif

/* Copy the contents of TREE to a new tree in POOL.  If COPY is
   non-NULL, then each data item is passed to function COPY, and the
   return values are inserted into the new tree; otherwise, the items
   are copied verbatim from the old tree to the new tree.  Returns the
   new tree. */
avl_tree *
avl_copy (MAYBE_POOL const avl_tree *tree, avl_copy_func copy)
{
  /* This is a combination of Knuth's Algorithm 2.3.1C (copying a
     binary tree) and Algorithm 2.3.1T as modified by exercise 12
     (preorder traversal). */

  avl_tree *new_tree;

  /* PT1. */
  const avl_node *pa[AVL_MAX_HEIGHT];	/* Stack PA: nodes. */
  const avl_node **pp = pa;		/* Stack PA: stack pointer. */
  const avl_node *p = &tree->root;
  
  /* QT1. */
  avl_node *qa[AVL_MAX_HEIGHT];	/* Stack QA: nodes. */
  avl_node **qp = qa;		/* Stack QA: stack pointer. */
  avl_node *q;
  
  if (!(tree != NULL)) error("assert failed : tree != NULL");
#if PSPP
  new_tree = avl_create (pool, tree->cmp, tree->param);
#else
  new_tree = avl_create (tree->cmp, tree->param);
#endif
  new_tree->count = tree->count;
  q = &new_tree->root;

  for (;;)
    {
      /* C4. */
      if (p->link[0] != NULL)
	{
	  avl_node *r = new_node (pool);
	  r->link[0] = r->link[1] = NULL;
	  q->link[0] = r;
	}

      /* C5: Find preorder successors of P and Q.  */
      goto start;
      for (;;)
	{
	  /* PT2. */
	  while (p != NULL)
	    {
	      goto escape;
	    start:
	      /* PT3. */
	      *pp++ = p;
	      *qp++ = q;
	      p = p->link[0];
	      q = q->link[0];
	    }
      
	  /* PT4. */
	  if (pp == pa)
	    {
	      if (!(qp == qa)) error("assert failed : qp == qa");
	      return new_tree;
	    }
	      
	  p = *--pp;
	  q = *--qp;

	  /* PT5. */
	  p = p->link[1];
	  q = q->link[1];
	}
    escape:

      /* C2. */
      if (p->link[1])
	{
	  avl_node *r = new_node (pool);
	  r->link[0] = r->link[1] = NULL;
	  q->link[1] = r;
	}

      /* C3. */
      q->bal = p->bal;
      if (copy == NULL)
	q->data = p->data;
      else
	q->data = copy (p->data, tree->param);
    }
}

/* Walk tree TREE in inorder, calling WALK_FUNC at each node.  Passes
   PARAM to WALK_FUNC.  */
void
avl_walk (const avl_tree *tree, avl_node_func walk_func, void *param)
{
  /* Uses Knuth's algorithm 2.3.1T (inorder traversal). */
  if (!(tree && walk_func)) error("assert failed : tree && walk_func");
  
  {
    /* T1. */
    const avl_node *an[AVL_MAX_HEIGHT];	/* Stack A: nodes. */
    const avl_node **ap = an;		/* Stack A: stack pointer. */
    const avl_node *p = tree->root.link[0];

    for (;;)
      {
	/* T2. */
	while (p != NULL)
	  {
	    /* T3. */
	    *ap++ = p;
	    p = p->link[0];
	  }
      
	/* T4. */
	if (ap == an)
	  return;
	p = *--ap;

	/* T5. */
	walk_func (p->data, param);
	p = p->link[1];
      }
  }
}

/* Each call to this function for a given TREE and TRAV return the
   next item in the tree in inorder.  Initialize the first element of
   TRAV (init) to 0 before calling the first time.  Returns NULL when
   out of elements.  */
void *
avl_traverse (const avl_tree *tree, avl_traverser *trav)
{
  if (!(tree && trav)) error("assert failed : tree && trav");

  /* Uses Knuth's algorithm 2.3.1T (inorder traversal). */
  if (trav->init == 0)
    {
      /* T1. */
      trav->init = 1;
      trav->nstack = 0;
      trav->p = tree->root.link[0];
    }
  else
    /* T5. */
    trav->p = trav->p->link[1];

  for (;;)
    {
      /* T2. */
      while (trav->p != NULL)
	{
	  /* T3. */
	  trav->stack[trav->nstack++] = trav->p;
	  trav->p = trav->p->link[0];
	}
      
      /* T4. */
      if (trav->nstack == 0)
	{
	  trav->init = 0;
	  return NULL;
	}
      trav->p = trav->stack[--trav->nstack];

      /* T5. */
      return trav->p->data;
    }
}

/* Search TREE for an item matching ITEM.  If found, returns a pointer
   to the address of the item.  If none is found, ITEM is inserted
   into the tree, and a pointer to the address of ITEM is returned.
   In either case, the pointer returned can be changed by the caller,
   or the returned data item can be directly edited, but the key data
   in the item must not be changed. */
void **
avl_probe (avl_tree *tree, void *item)
{
  /* Uses Knuth's Algorithm 6.2.3A (balanced tree search and
     insertion), but caches results of comparisons.  In empirical
     tests this eliminates about 25% of the comparisons seen under
     random insertions.  */

  /* A1. */
  avl_node *t;
  avl_node *s, *p, *q, *r;
  
  if (!(tree != NULL)) error("assert failed : tree != NULL");
  t = &tree->root;
  s = p = t->link[0];

  if (s == NULL)
    {
      tree->count++;
      if (!(tree->count == 1)) error("assert failed : tree->count == 1");
      q = t->link[0] = new_node (tree->pool);
      q->data = item;
      q->link[0] = q->link[1] = NULL;
      q->bal = 0;
      return &q->data;
    }

  for (;;)
    {
      /* A2. */
      int diff = tree->cmp (item, p->data, tree->param);

      /* A3. */
      if (diff < 0)
	{
	  p->cache = 0;
	  q = p->link[0];
	  if (q == NULL)
	    {
	      p->link[0] = q = new_node (tree->pool);
	      break;
	    }
	}
      /* A4. */
      else if (diff > 0)
	{
	  p->cache = 1;
	  q = p->link[1];
	  if (q == NULL)
	    {
	      p->link[1] = q = new_node (tree->pool);
	      break;
	    }
	}
      else
	/* A2. */
	return &p->data;

      /* A3, A4. */
      if (q->bal != 0)
	t = p, s = q;
      p = q;
    }
  
  /* A5. */
  tree->count++;
  q->data = item;
  q->link[0] = q->link[1] = NULL;
  q->bal = 0;

  /* A6. */
  r = p = s->link[(int) s->cache];
  while (p != q)
    {
      p->bal = p->cache * 2 - 1;
      p = p->link[(int) p->cache];
    }

  /* A7. */
  if (s->cache == 0)
    {
      /* a = -1. */
      if (s->bal == 0)
	{
	  s->bal = -1;
	  return &q->data;
	}
      else if (s->bal == +1)
	{
	  s->bal = 0;
	  return &q->data;
	}
      
      if (!(s->bal == -1)) error("assert failed : s->bal == -1");
      if (r->bal == -1)
	{
	  /* A8. */
	  p = r;
	  s->link[0] = r->link[1];
	  r->link[1] = s;
	  s->bal = r->bal = 0;
	}
      else
	{
	  /* A9. */
	  if (!(r->bal == +1)) error("assert failed : r->bal == +1");
	  p = r->link[1];
	  r->link[1] = p->link[0];
	  p->link[0] = r;
	  s->link[0] = p->link[1];
	  p->link[1] = s;
	  if (p->bal == -1)
	    s->bal = 1, r->bal = 0;
	  else if (p->bal == 0)
	    s->bal = r->bal = 0;
	  else 
	    {
	      if (!(p->bal == +1)) error("assert failed : p->bal == +1");
	      s->bal = 0, r->bal = -1;
	    }
	  p->bal = 0;
	}
    }
  else
    {
      /* a == +1. */
      if (s->bal == 0)
	{
	  s->bal = 1;
	  return &q->data;
	}
      else if (s->bal == -1)
	{
	  s->bal = 0;
	  return &q->data;
	}

      if (!(s->bal == +1)) error("assert failed : s->bal == +1");
      if (r->bal == +1)
	{
	  /* A8. */
	  p = r;
	  s->link[1] = r->link[0];
	  r->link[0] = s;
	  s->bal = r->bal = 0;
	}
      else
	{
	  /* A9. */
	  if (!(r->bal == -1)) error("assert failed : r->bal == -1");
	  p = r->link[0];
	  r->link[0] = p->link[1];
	  p->link[1] = r;
	  s->link[1] = p->link[0];
	  p->link[0] = s;
	  if (p->bal == +1)
	    s->bal = -1, r->bal = 0;
	  else if (p->bal == 0)
	    s->bal = r->bal = 0;
	  else 
	    {
	      if (!(p->bal == -1)) error("assert failed : p->bal == -1");
	      s->bal = 0, r->bal = 1;
	    }
	  p->bal = 0;
	}
    }
		
  /* A10. */
  if (t != &tree->root && s == t->link[1])
    t->link[1] = p;
  else
    t->link[0] = p;

  return &q->data;
}
  
/* Search TREE for an item matching ITEM, and return it if found. */
void *
avl_find (const avl_tree *tree, const void *item)
{
  const avl_node *p;

  if (!(tree != NULL)) error("assert failed : tree != NULL");
  for (p = tree->root.link[0]; p; )
    {
      int diff = tree->cmp (item, p->data, tree->param);

      if (diff < 0)
	p = p->link[0];
      else if (diff > 0)
	p = p->link[1];
      else
	return p->data;
    }

  return NULL;
}

/* Searches AVL tree TREE for an item matching ITEM.  If found, the
   item is removed from the tree and the actual item found is returned
   to the caller.  If no item matching ITEM exists in the tree,
   returns NULL. */
void *
avl_delete (avl_tree *tree, const void *item)
{
  /* Uses my Algorithm D, which can be found at
     http://www.msu.edu/user/pfaffben/avl.  Algorithm D is based on
     Knuth's Algorithm 6.2.2D (Tree deletion) and 6.2.3A (Balanced
     tree search and insertion), as well as the notes on pages 465-466
     of Vol. 3. */

  /* D1. */
  avl_node *pa[AVL_MAX_HEIGHT];		/* Stack P: Nodes. */
  char a[AVL_MAX_HEIGHT];		/* Stack P: Bits. */
  int k = 1;				/* Stack P: Pointer. */
  
  avl_node **q;
  avl_node *p;

  if (!(tree != NULL)) error("assert failed : tree != NULL");

  a[0] = 0;
  pa[0] = &tree->root;
  p = tree->root.link[0];
  for (;;)
    {
      /* D2. */
      int diff;

      if (p == NULL)
	return NULL;

      diff = tree->cmp (item, p->data, tree->param);
      if (diff == 0)
	break;

      /* D3, D4. */
      pa[k] = p;
      if (diff < 0)
	{
	  p = p->link[0];
	  a[k] = 0;
	}
      else if (diff > 0)
	{
	  p = p->link[1];
	  a[k] = 1;
	}
      k++;
    }
  tree->count--;
  
  item = p->data;

  /* D5. */
  q = &pa[k - 1]->link[(int) a[k - 1]];
  if (p->link[1] == NULL)
    {
      *q = p->link[0];
      if (*q)
	(*q)->bal = 0;
    }
  else
    {
      /* D6. */
      avl_node *r = p->link[1];
      if (r->link[0] == NULL)
	{
	  r->link[0] = p->link[0];
	  *q = r;
	  r->bal = p->bal;
	  a[k] = 1;
	  pa[k++] = r;
	}
      else
	{
	  /* D7. */
	  avl_node *s = r->link[0];
	  int l = k++;

	  a[k] = 0;
	  pa[k++] = r;
	    
	  /* D8. */
	  while (s->link[0] != NULL)
	    {
	      r = s;
	      s = r->link[0];
	      a[k] = 0;
	      pa[k++] = r;
	    }

	  /* D9. */
	  a[l] = 1;
	  pa[l] = s;
	  s->link[0] = p->link[0];
	  r->link[0] = s->link[1];
	  s->link[1] = p->link[1];
	  s->bal = p->bal;
	  *q = s;
	}
    }

#if PSPP
  if (tree->pool == NULL)
#endif
    Free (p);

  if (!(k > 0)) error("assert failed : k > 0");
  /* D10. */
  while (--k)
    {
      avl_node *s = pa[k], *r;

      if (a[k] == 0)
	{
	  /* D10. */
	  if (s->bal == -1)
	    {
	      s->bal = 0;
	      continue;
	    }
	  else if (s->bal == 0)
	    {
	      s->bal = 1;
	      break;
	    }

	  if (!(s->bal == +1)) error("assert failed : s->bal == +1");
	  r = s->link[1];

	  if (!(r != NULL)) error("assert failed : r != NULL");
	  if (r->bal == 0)
	    {
	      /* D11. */
	      s->link[1] = r->link[0];
	      r->link[0] = s;
	      r->bal = -1;
	      pa[k - 1]->link[(int) a[k - 1]] = r;
	      break;
	    }
	  else if (r->bal == +1)
	    {
	      /* D12. */
	      s->link[1] = r->link[0];
	      r->link[0] = s;
	      s->bal = r->bal = 0;
	      pa[k - 1]->link[(int) a[k - 1]] = r;
	    }
	  else 
	    {
	      /* D13. */
	      if (!(r->bal == -1)) error("assert failed : r->bal == -1");
	      p = r->link[0];
	      r->link[0] = p->link[1];
	      p->link[1] = r;
	      s->link[1] = p->link[0];
	      p->link[0] = s;
	      if (p->bal == +1)
		s->bal = -1, r->bal = 0;
	      else if (p->bal == 0)
		s->bal = r->bal = 0;
	      else
		{
		  if (!(p->bal == -1)) error("assert failed : p->bal == -1");
		  s->bal = 0, r->bal = +1;
		}
	      p->bal = 0;
	      pa[k - 1]->link[(int) a[k - 1]] = p;
	    }
	}
      else
	{
	  if (!(a[k] == 1)) error("assert failed : a[k] == 1");

	  /* D10. */
	  if (s->bal == +1)
	    {
	      s->bal = 0;
	      continue;
	    }
	  else if (s->bal == 0)
	    {
	      s->bal = -1;
	      break;
	    }

	  if (!(s->bal == -1)) error("assert failed : s->bal == -1");
	  r = s->link[0];

	  if (r == NULL || r->bal == 0)
	    {
	      /* D11. */
	      s->link[0] = r->link[1];
	      r->link[1] = s;
	      r->bal = 1;
	      pa[k - 1]->link[(int) a[k - 1]] = r;
	      break;
	    }
	  else if (r->bal == -1)
	    {
	      /* D12. */
	      s->link[0] = r->link[1];
	      r->link[1] = s;
	      s->bal = r->bal = 0;
	      pa[k - 1]->link[(int) a[k - 1]] = r;
	    }
	  else if (r->bal == +1)
	    {
	      /* D13. */
	      p = r->link[1];
	      r->link[1] = p->link[0];
	      p->link[0] = r;
	      s->link[0] = p->link[1];
	      p->link[1] = s;
	      if (p->bal == -1)
		s->bal = 1, r->bal = 0;
	      else if (p->bal == 0)
		s->bal = r->bal = 0;
	      else
		{
		  if (!(p->bal == 1)) error("assert failed : p->bal == 1");
		  s->bal = 0, r->bal = -1;
		}
	      p->bal = 0;
	      pa[k - 1]->link[(int) a[k - 1]] = p;
	    }
	}
    }
      
  return (void *) item;
}

/* Inserts ITEM into TREE.  Returns NULL if the item was inserted,
   otherwise a pointer to the duplicate item. */
void *
avl_insert (avl_tree *tree, void *item)
{
  void **p;
  
  if (!(tree != NULL)) error("assert failed : tree != NULL");
  
  p = avl_probe (tree, item);
  return (*p == item) ? NULL : *p;
}

/* If ITEM does not exist in TREE, inserts it and returns NULL.  If a
   matching item does exist, it is replaced by ITEM and the item
   replaced is returned.  The caller is responsible for freeing the
   item returned. */
void *
avl_replace (avl_tree *tree, void *item)
{
  void **p;

  if (!(tree != NULL)) error("assert failed : tree != NULL");
  
  p = avl_probe (tree, item);
  if (*p == item)
    return NULL;
  else
    {
      void *r = *p;
      *p = item;
      return r;
    }
}

/* Delete ITEM from TREE when you know that ITEM must be in TREE.  For
   debugging purposes. */
void *
(avl_force_delete) (avl_tree *tree, void *item)
{
  void *found = avl_delete (tree, item);
  if (!(found != NULL)) error("assert failed : found != NULL");
  return found;
}

#if SELF_TEST

/* Used to flag delayed aborting. */
int done = 0;

/* Print the structure of node NODE of an avl tree, which is LEVEL
   levels from the top of the tree.  Uses different delimiters to
   visually distinguish levels. */
void
print_structure (avl_node *node, int level)
{
  char lc[] = "([{`/";
  char rc[] = ")]}'\\";

  if (!(level <= 10)) error("assert failed : level <= 10");
  
  if (node == NULL)
    {
      printf (" nil");
      return;
    }
  printf (" %c%d", lc[level % 5], (int) node->data);
  if (node->link[0] || node->link[1])
    print_structure (node->link[0], level + 1);
  if (node->link[1])
    print_structure (node->link[1], level + 1);
  printf ("%c", rc[level % 5]);
}

/* Compare two integers A and B and return a strcmp()-type result. */
int
compare_ints (const void *a, const void *b, void *param unused)
{
  return ((int) a) - ((int) b);
}

/* Print the value of integer A. */
void
print_int (void *a, void *param unused)
{
  printf (" %d", (int) a);
}

/* Linearly print contents of TREE. */
void
print_contents (avl_tree *tree)
{
  avl_walk (tree, print_int, NULL);
  printf ("\n");
}

/* Examine NODE in a avl tree.  *COUNT is increased by the number of
   nodes in the tree, including the current one.  If the node is the
   root of the tree, PARENT should be INT_MIN, otherwise it should be
   the parent node value.  DIR is the direction that the current node
   is linked from the parent: -1 for left child, +1 for right child;
   it is not used if PARENT is INT_MIN.  Returns the height of the
   tree rooted at NODE. */
int
recurse_tree (avl_node *node, int *count, int parent, int dir)
{
  if (node) 
    {
      int d = (int) node->data;
      int nl = node->link[0] ? recurse_tree (node->link[0], count, d, -1) : 0;
      int nr = node->link[1] ? recurse_tree (node->link[1], count, d, 1) : 0;
      (*count)++;

      if (nr - nl != node->bal)
	{
	  printf (" Node %d is unbalanced: right height=%d, left height=%d, "
		"difference=%d, but balance factor=%d.\n",
		  d, nr, nl, nr - nl, node->bal);
	  done = 1;
	}
      
      if (parent != INT_MIN)
	{
	  if (!(dir == -1 || dir == +1)) error("assert failed : dir == -1 || dir == +1");
	  if (dir == -1 && d > parent)
	    {
	      printf (" Node %d is smaller than its left child %d.\n",
		      parent, d);
	      done = 1;
	    }
	  else if (dir == +1 && d < parent)
	    {
	      printf (" Node %d is larger than its right child %d.\n",
		      parent, d);
	      done = 1;
	    }
	}
      if (!(node->bal >= -1 && node->bal <= 1)) error("assert failed : node->bal >= -1 && node->bal <= 1");
      return 1 + (nl > nr ? nl : nr);
    }
  else return 0;
}

/* Check that everything about TREE is kosher. */
void
verify_tree (avl_tree *tree)
{
  int count = 0;
  recurse_tree (tree->root.link[0], &count, INT_MIN, 0);
  if (count != tree->count)
    {
      printf (" Tree has %d nodes, but tree count is %d.\n",
	      count, tree->count);
      done = 1;
    }
  if (done)
    abort ();
}

/* Arrange the N elements of ARRAY in random order. */
void
shuffle (int *array, int n)
{
  int i;
  
  for (i = 0; i < n; i++)
    {
      int j = i + rand () % (n - i);
      int t = array[j];
      array[j] = array[i];
      array[i] = t;
    }
}

/* Compares avl trees rooted at A and B, making sure that they are
   identical. */
void
compare_trees (avl_node *a, avl_node *b)
{
  if (a == NULL || b == NULL)
    {
      if (!(a == NULL && b == NULL)) error("assert failed : a == NULL && b == NULL");
      return;
    }
  if (a->data != b->data || a->bal != b->bal
      || ((a->link[0] != NULL) ^ (b->link[0] != NULL))
      || ((a->link[1] != NULL) ^ (b->link[1] != NULL)))
    {
      printf (" Copied nodes differ: %d b=%d a->bal=%d b->bal=%d a:",
	      (int) a->data, (int) b->data, a->bal, b->bal);
      if (a->link[0])
	printf ("l");
      if (a->link[1])
	printf ("r");
      printf (" b:");
      if (b->link[0])
	printf ("l");
      if (b->link[1])
	printf ("r");
      printf ("\n");
      abort ();
    }
  if (a->link[0] != NULL)
    compare_trees (a->link[0], b->link[0]);
  if (a->link[1] != NULL)
    compare_trees (a->link[1], b->link[1]);
}

/* Simple stress test procedure for the AVL tree routines.  Does the
   following:

   * Generate a random number seed.  By default this is generated from
   the current time.  You can also pass a seed value on the command
   line if you want to test the same case.  The seed value is
   displayed.

   * Create a tree and insert the integers from 0 up to TREE_SIZE - 1
   into it, in random order.  Verify the tree structure after each
   insertion.
   
   * Remove each integer from the tree, in a different random order.
   After each deletion, verify the tree structure; also, make a copy
   of the tree into a new tree, verify the copy and compare it to the
   original, then destroy the copy.

   * Destroy the tree, increment the random seed value, and start over.

   If you make any modifications to the avl tree routines, then you
   might want to insert some calls to print_structure() at strategic
   places in order to be able to see what's really going on.  Also,
   memory debuggers like Checker or Purify are very handy. */
#define TREE_SIZE 1024
#define N_ITERATIONS 16
int
main (int argc, char **argv)
{
  int array[TREE_SIZE];
  int seed;
  int iteration;
  
  if (argc == 2)
    seed = atoi (argv[1]);
  else
    seed = time (0) * 257 % 32768;

  fputs ("Testing avl...\n", stdout);
  
  for (iteration = 1; iteration <= N_ITERATIONS; iteration++)
    {
      avl_tree *tree;
      int i;
      
      printf ("Iteration %4d/%4d: seed=%5d", iteration, N_ITERATIONS, seed);
      fflush (stdout);
      
      srand (seed++);

      for (i = 0; i < TREE_SIZE; i++)
	array[i] = i;
      shuffle (array, TREE_SIZE);
      
      tree = avl_create (compare_ints, NULL);
      for (i = 0; i < TREE_SIZE; i++)
	avl_force_insert (tree, (void *) (array[i]));
      verify_tree (tree);

      shuffle (array, TREE_SIZE);
      for (i = 0; i < TREE_SIZE; i++)
	{
	  avl_tree *copy;

	  avl_delete (tree, (void *) (array[i]));
	  verify_tree (tree);

	  copy = avl_copy (tree, NULL);
	  verify_tree (copy);
	  compare_trees (tree->root.link[0], copy->root.link[0]);
	  avl_destroy (copy, NULL);

	  if (i % 128 == 0)
	    {
	      putchar ('.');
	      fflush (stdout);
	    }
	}
      fputs (" good.\n", stdout);

      avl_destroy (tree, NULL);
    }
  
  return 0;
}
#endif /* SELF_TEST */

/*
  Local variables:
  compile-command: "gcc -DSELF_TEST=1 -W -Wall -I. -o ./avl-test avl.c"
  End:
*/

