
// CUDD Stubs
#include <stdio.h>
#include <assert.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/custom.h>
#include <caml/callback.h>
#include <cudd_stubs.h>

/** Manager **/

/* Custom functions */
void custom_manager_finalize(value v) 
{
  mlcuddManagerFree(Manager_val(v));
}

int custom_manager_compare(value v1, value v2)
{
  DdManager *m1 = Manager_val(v1)->manager;
  DdManager *m2 = Manager_val(v2)->manager;
  return ((long)m1==(long)m2 ? 0 : (long)m1<(long)m2 ? -1 : 1);
}

long custom_manager_hash(value v)
{
  DdManager *m = Manager_val(v)->manager;
  long hash = (long)m;
  return hash;
}

/* Encapsulation of opaque handles */
static struct custom_operations cudd_manager_ops = {
  "org.imdea.software.ares.rounded.cudd_managers",
  custom_manager_finalize,
  custom_manager_compare,
  custom_manager_hash,
  custom_serialize_default,
  custom_deserialize_default
};

/* Allocating an Ocaml custom block to hold the manager_t * */
static value alloc_manager(manager_t *manager)
{
  value v;
  
  if (NULL == manager || NULL == manager->manager)
    caml_failwith("Cudd: null manager.");
  
  v = caml_alloc_custom(&cudd_manager_ops, sizeof(manager_t), 1, 10);
  Manager_val(v) = manager;
  mlcuddManagerRef(manager);

  return v;
}

CAMLprim value caml_cudd_manager_make(value unit)
{
  CAMLparam1 (unit);
  manager_t *manager = mlcuddManagerInit(0,0,0,0,0);
  
  CAMLreturn (alloc_manager(manager));
}


/** BDD nodes **/

/* Custom functions */
void custom_bdd_finalize(value v)
{
  node_t *node = Node_val(v);

  //assert (Cudd_Regular(node->node)->ref >= 1);
  
  //printf("\ncudd_stubs.c: Node(%lx, %d, %d) will be deleted.\n", 
  //	 (unsigned long)node, Cudd_Regular(node->node)->index,
  //	 Cudd_Regular(node->node)->ref);
  //printf("cudd_stubs.c: DdNode: %lx\n", (unsigned long)(node->node));

  Cudd_RecursiveDeref(node->manager->manager, node->node);
  //DdNode *n = Cudd_Regular(_node);
  //printf("cudd_stubs::n(%lx)->ref = %d\n", (unsigned long)n, n->ref);
  mlcuddManagerFree(node->manager);

  /*XXX DEBUG */
  //printf("cudd_stubs.c: Node(%lx, %d) deleted.\n", 
  //	 (unsigned long)node, Cudd_Regular(node->node)->index);
  /*XXX DEBUG */
}

int custom_bdd_compare(value v1, value v2)
{
  int res;

  bdd_t *b1 = Node_val(v1);
  bdd_t *b2 = Node_val(v2);

  DdManager *m1 = b1->manager->manager;
  DdManager *m2 = b2->manager->manager;
  DdNode *n1 = b1->node;
  DdNode *n2 = b2-> node;

  res = (long)m1==(long)m2 ? 0 : (long)m1<(long)m2 ? -1 : 1;
  
  if (0 == res)
    res = (long)n1==(long)n2 ? 0 : (long)n1<(long)n2 ? -1 : 1;

  return res;
}

long custom_bdd_hash(value v)
{
  DdNode *n = Node_val(v)->node;
  long hash = (long)n;
  return hash;
}

/* Encapsulation of opaque handles */
static struct custom_operations cudd_bdd_ops = {
  "org.imdea.software.ares.rounded.cudd_bdds",
  custom_bdd_finalize,
  custom_bdd_compare,
  custom_bdd_hash,
  custom_serialize_default,
  custom_deserialize_default
};

/* Allocating an Ocaml custom block to hold the bdd_t */
static value alloc_bdd(bdd_t *bdd)
{
  value v;
  
  if (NULL == bdd->manager)
    caml_failwith("Cudd: manager is null.");

  if (NULL == bdd->node) {
    Cudd_ErrorType err = Cudd_ReadErrorCode(bdd->manager->manager);
    char *s, err_msg[128];
    switch (err) {
    case CUDD_NO_ERROR: s = "CUDD_NO_ERROR"; break;
    case CUDD_MEMORY_OUT: s = "CUDD_MEMORY_OUT"; break;
    case CUDD_TOO_MANY_NODES: s = "CUDD_TOO_MANY_NODES"; break;
    case CUDD_MAX_MEM_EXCEEDED: s = "CUDD_MAX_MEM_EXCEEDED"; break;
    case CUDD_INVALID_ARG: s = "CUDD_INVALID_ARG"; break;
    case CUDD_INTERNAL_ERROR: s = "CUDD_INTERNAL_ERROR"; break;
    default: s = "CUDD_UNKNOWN"; break;
    }
    sprintf(err_msg, "Cudd: null bdd node. Error code: %s\n", s);
    caml_failwith(err_msg);
  }
  
  Cudd_Ref(bdd->node);
  mlcuddManagerRef(bdd->manager);

  /*XXX DEBUG */
  //DdNode *n = Cudd_Regular(bdd->node);
  //printf("cudd_stubs.c: Node(%lx, %lx, %d) created.\n", 
  //	 (unsigned long)bdd, (unsigned long)n, n->index);
  /*XXX DEBUG */

  v = caml_alloc_custom(&cudd_bdd_ops, sizeof(bdd_t), 1, 5);
  *Node_val(v) = *bdd;

  return v;
}


/** Bdd functions **/

CAMLprim value caml_cudd_bdd_dtrue(value mgr)
{
  CAMLparam1 (mgr);
  
  bdd_t res;
  manager_t *manager = Manager_val(mgr);

  res.node = DD_ONE(manager->manager);
  res.manager = manager;

  CAMLreturn (alloc_bdd(&res));
}

CAMLprim value caml_cudd_bdd_dfalse(value mgr)
{
  CAMLparam1 (mgr);
  
  bdd_t res;
  manager_t *manager = Manager_val(mgr);

  res.node = Cudd_Not(DD_ONE(manager->manager));
  res.manager = manager;

  CAMLreturn (alloc_bdd(&res));
}

CAMLprim value caml_cudd_bdd_ithvar(value mgr, value i)
{
  CAMLparam2 (mgr, i);

  bdd_t bdd;
  bdd.manager = Manager_val(mgr);
  bdd.node = (DdNode *)Cudd_bddIthVar(bdd.manager->manager, Int_val(i));
  
  //printf("Cudd_bddIthVar\n");

  CAMLreturn (alloc_bdd(&bdd));
}

CAMLprim value caml_cudd_bdd_newvar(value mgr)
{
  CAMLparam1 (mgr);

  bdd_t res;
  res.manager = Manager_val(mgr);
  res.node = Cudd_bddNewVar(res.manager->manager);

  //printf("Cudd_bddNewVar\n");
  
  CAMLreturn (alloc_bdd(&res));
}

CAMLprim value caml_cudd_bdd_newvar_at_level(value mgr, value level)
{
  CAMLparam2 (mgr, level);

  bdd_t res;

  res.manager = Manager_val(mgr);
  res.node = 
    Cudd_bddNewVarAtLevel(res.manager->manager, Int_val(level));

  //printf("Cudd_bddNewVarAtLevel\n");

  CAMLreturn (alloc_bdd(&res));
}

CAMLprim value caml_cudd_bdd_index(value node)
{
  CAMLparam1 (node);

  int index = Node_val(node)->node->index;

  CAMLreturn (Val_int(index));
}

CAMLprim value caml_cudd_bdd_is_true(value node)
{
  CAMLparam1 (node);
  
  node_t *_node;
  int res;
  
  _node = Node_val(node);
  res = (_node->node == DD_ONE(_node->manager->manager));

  CAMLreturn (Val_int(res));
}

CAMLprim value caml_cudd_bdd_is_false(value node)
{
  CAMLparam1 (node);
  
  node_t *_node;
  int res;
  
  _node = Node_val(node);
  res = (_node->node == Cudd_Not(DD_ONE(_node->manager->manager)));

  CAMLreturn (Val_int(res));
}

CAMLprim value caml_cudd_bdd_dnot(value node)
{
  CAMLparam1 (node);

  bdd_t *_node = Node_val(node);
  bdd_t res;
  
  res.node = Cudd_Not(_node->node);
  res.manager = _node->manager;

  //printf("Cudd_Not\n");
  
  CAMLreturn (alloc_bdd(&res));
}


CAMLprim value caml_cudd_bdd_dand(value n1, value n2)
{
  CAMLparam2 (n1, n2);

  bdd_t res;

  mlcuddBinaryOper(Cudd_bddAnd, Node_val(n1), Node_val(n2), &res);
  
  //printf("Cudd_bddAnd\n");

  CAMLreturn (alloc_bdd(&res)); 
}

CAMLprim value caml_cudd_bdd_dor(value n1, value n2)
{
  CAMLparam2 (n1, n2);

  bdd_t res;

  mlcuddBinaryOper(Cudd_bddOr, Node_val(n1), Node_val(n2), &res);
  
  //printf("Cudd_bddOr\n");

  CAMLreturn (alloc_bdd(&res)); 
}

CAMLprim value caml_cudd_bdd_compose(value f, value g, value v)
{
  CAMLparam3 (f, g, v);

  bdd_t *_f, *_g, res;
  int _v;

  _f = Node_val(f);
  _g = Node_val(g);
  _v = Int_val(v);

  if (_f->manager != _g->manager )
    caml_failwith("Cudd: nodes belong to different managers.");

  res.node = Cudd_bddCompose(_f->manager->manager, _f->node, _g->node, _v);
  res.manager = _f->manager;

  CAMLreturn(alloc_bdd(&res));
}

CAMLprim value caml_cudd_bdd_restrict(value n1, value n2)
{
  CAMLparam2 (n1, n2);

  bdd_t res;

  mlcuddBinaryOper(Cudd_bddRestrict, Node_val(n1), Node_val(n2), &res);
  
  //printf("Cudd_bddRestrict\n");

  CAMLreturn (alloc_bdd(&res)); 
}

CAMLprim value caml_cudd_bdd_constrain(value n1, value n2)
{
  CAMLparam2 (n1, n2);

  bdd_t res;

  mlcuddBinaryOper(Cudd_bddConstrain, Node_val(n1), Node_val(n2), &res);
  
  //printf("Cudd_bddConstrain\n");

  CAMLreturn (alloc_bdd(&res)); 
}

CAMLprim value caml_cudd_bdd_find_essential(value node)
{
  CAMLparam1 (node);
  
  bdd_t *_node = Node_val(node);
  bdd_t res; 
  
  res.node = Cudd_FindEssential(_node->manager->manager, _node->node);
  res.manager = _node->manager;
  
  CAMLreturn (alloc_bdd(&res));
}

CAMLprim value caml_cudd_bdd_iter_cube(value f_closure, value node)
{
  CAMLparam2 (f_closure, node);
  CAMLlocal1(val_cube);
  
  bdd_t *_node;
  DdManager *_manager;
  DdGen *gen;
  int *cube;
  double val;
  int size, i;
  int autodyn;
  Cudd_ReorderingType heuristic;

  _node = Node_val(node);
  _manager = _node->manager->manager;

  autodyn = 0;
  if (Cudd_ReorderingStatus(_manager, &heuristic)) {
    autodyn = 1;
    Cudd_AutodynDisable(_manager);
  }
  size = _manager->size;

  Cudd_ForeachCube(_manager,_node->node, gen, cube, val)
    {
      if (size==0) {
	val_cube = Atom(0);
      }
      else {
	val_cube = caml_alloc(size, 0);
	for(i=0; i< size; i++){
	  Store_field(val_cube, i, Val_int(cube[i]));
	}
      }
      caml_callback(f_closure, val_cube);
    }
  
  if (autodyn) 
    Cudd_AutodynEnable(_manager, CUDD_REORDER_SAME);

  CAMLreturn(Val_unit);
}

CAMLprim value caml_cudd_bdd_cubes(value node)
{
  CAMLparam1 (node);
  CAMLlocal3 (val_cube, val_cube_list, val_cube_tmp);
  
  bdd_t *_node;
  DdManager *_manager;
  DdGen *gen;
  int *cube;
  double val;
  int size, i;
  int autodyn;
  Cudd_ReorderingType heuristic;

  _node = Node_val(node);
  _manager = _node->manager->manager;

  autodyn = 0;
  if (Cudd_ReorderingStatus(_manager, &heuristic)) {
    autodyn = 1;
    Cudd_AutodynDisable(_manager);
  }
  size = _manager->size;
  
  val_cube_list = Val_int(0); // Empty lis of cubes

  Cudd_ForeachCube(_manager,_node->node, gen, cube, val)
    {
      if (size==0) {
	val_cube = Atom(0);
      }
      else {
	val_cube = caml_alloc(size, 0);

	for(i=0; i< size; i++){
	  Store_field(val_cube, i, Val_int(cube[i]));
	}
      }
      
      /* Save the cube into the list of cubes */
      val_cube_tmp = caml_alloc_small(2,0);
      Field(val_cube_tmp, 0) = val_cube;
      Field(val_cube_tmp, 1) = val_cube_list;
      val_cube_list = val_cube_tmp;
    }
  
  if (autodyn) 
    Cudd_AutodynEnable(_manager, CUDD_REORDER_SAME);

  CAMLreturn(val_cube_list);
}

DdNode *bddComplementCube(DdManager *manager, int *cube, int size) {
  int i;
  DdNode *res, *node;
  
  res = DD_ONE(manager);
  for (i=0; i<size; i++) {
    if ( 2 == cube[i]) continue;

    node = Cudd_bddIthVar(manager, i);
    
    if (1 == cube[i]) 
      node = Cudd_Not(node);
    Cudd_Ref(node);
    
    res = Cudd_bddAnd(manager, res, node);
    Cudd_Ref(res);
  }
  
  return res;
}

CAMLprim value caml_cudd_bdd_restricted_cubes(value node)
{
  CAMLparam1 (node);
  CAMLlocal3 (val_cube, val_cube_list, val_cube_tmp);
  
  bdd_t *_node;
  DdManager *_manager;
  DdGen *gen;
  int *cube;
  double val;
  int size, i;
  int autodyn;
  Cudd_ReorderingType heuristic;
  DdNode *iter, *first, *zero;

  _node = Node_val(node);
  _manager = _node->manager->manager;

  autodyn = 0;
  if (Cudd_ReorderingStatus(_manager, &heuristic)) {
    autodyn = 1;
    Cudd_AutodynDisable(_manager);
  }
  size = _manager->size;
  
  val_cube_list = Val_int(0); // Empty lis of cubes

  iter = _node->node;
  zero = Cudd_Not(DD_ONE(_manager));

  while ( iter != zero ) {
    gen = Cudd_FirstCube(_manager, iter, &cube, &val);
    Cudd_IsGenEmpty(gen) ? Cudd_GenFree(gen) : TRUE;

    if (size == 0) {
      val_cube = Atom(0);
    }
    else {
      val_cube = caml_alloc(size,0);
      
      for(i=0; i< size; i++){
	Store_field(val_cube, i, Val_int(cube[i]));
      }
    }
    
    /* Save the cube into the list of cubes */
    val_cube_tmp = caml_alloc_small(2,0);
    Field(val_cube_tmp, 0) = val_cube;
    Field(val_cube_tmp, 1) = val_cube_list;
    val_cube_list = val_cube_tmp;
    
    first = bddComplementCube(_manager, cube, size);
    iter = Cudd_bddRestrict(_manager, iter, first);
    Cudd_RecursiveDeref(_manager, first);
  }
    
  if (autodyn) 
    Cudd_AutodynEnable(_manager, CUDD_REORDER_SAME);

  CAMLreturn(val_cube_list);
}
