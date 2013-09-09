
#ifndef __MLCUDD_H__
#define __MLCUDD_H__

#include <stdint.h>
#include <caml/fail.h>
#include "cudd.h"
#include "cuddInt.h"

/* Types */
struct manager__t {
  DdManager *manager;
  size_t count;
};

struct node__t {
  struct manager__t *manager;
  DdNode *node;
};

typedef struct manager__t manager_t;
typedef struct node__t node_t;

typedef DdNode *(*BinaryOper)(DdManager *, DdNode *, DdNode *);

/* Function Prototypes */

/* Manager */
manager_t *mlcuddManagerInit
(
 unsigned int numVars,
 unsigned int numVarsZ, 
 unsigned int numSlots, 
 unsigned int cacheSize,
 unsigned long maxMemory
 );

void mlcuddManagerFree(manager_t *manager);

#define mlcuddManagerRef(x) \
  do { if((x)->count<SIZE_MAX) (x)->count++; } while(0)

static inline manager_t *mlcuddManagerCopy(manager_t *manager)
{
  mlcuddManagerRef(manager);
  return manager;
}

void mlcuddBinaryOper(BinaryOper op, node_t *x, node_t *y, node_t *res);

#endif
