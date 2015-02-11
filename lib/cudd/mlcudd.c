
#include <stdio.h>
#include <assert.h>
#include <stdlib.h>
#include "mlcudd.h"
#include "util.h"

manager_t *mlcuddManagerInit 
(
 unsigned int numVars,
 unsigned int numVarsZ, 
 unsigned int numSlots, 
 unsigned int cacheSize,
 unsigned long maxMemory
 )
{
  if (numVars < 0) numVars = 0;
  if (numVarsZ < 0) numVarsZ = 0;
  if (numSlots < 0) numSlots = CUDD_UNIQUE_SLOTS;
  if (cacheSize < 0) cacheSize = CUDD_CACHE_SLOTS;
  if (maxMemory < 0) maxMemory = 0;

  manager_t *mgr = (manager_t *)malloc(sizeof(manager_t));
  if (NULL == mgr) 
    return NULL;
  
  mgr->manager = 
    Cudd_Init(numVars, numVarsZ, numSlots, cacheSize, maxMemory);
  mgr->count = 0;

  /*XXX DEBUG */
  //printf("mlcudd.c: Manager (%lx) created.\n", (unsigned long)mgr);
  /*XXX DEBUG */

  return mgr;
}

void mlcuddManagerFree(manager_t *manager)
{
  assert(manager->count>=1);

  if (manager->count<=1){

    assert(Cudd_CheckZeroRef(manager->manager)==0);
    Cudd_Quit(manager->manager);
    
    /*XXX DEBUG */
    //printf("mlcudd.c: Manager (%lx) deleted.\n", 
    //	   (unsigned long)manager);
    /*XXX DEBUG */

    free(manager);
  }
  else if (manager->count != SIZE_MAX) {
    manager->count--;
    
    /*XXX DEBUG */
    //printf("mlcudd.c: Manager (%lx) count: %ld\n", 
    //       (unsigned long)manager, manager->count);
    /*XXX DEBUG */
  }
}


void mlcuddBinaryOper(BinaryOper op, node_t *x, node_t *y, node_t *res)
{
  if ( x->manager != y->manager )
    caml_failwith("Cudd: nodes belong to different managers.");

  res->node = (*op)(x->manager->manager, x->node, y->node);
  res->manager = x->manager;
}
