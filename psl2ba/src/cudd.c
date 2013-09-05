#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/custom.h>
#include <caml/callback.h>
#include <stdio.h>

// Files from the CUDD library
#include "util.h"
#include "cudd.h"


CAMLprim value hello_init(value unit)
{
	CAMLparam1(unit);
	printf("hello, world\n");
	CAMLreturn(Val_int(7));
}

CAMLprim value call_caml(value fun, value fun2) 
{
  CAMLparam2(fun, fun2);
	CAMLlocal1(arr);

	caml_callback(fun, Val_int(5));

  arr = caml_alloc (3, 0);
  Store_field(arr, 0, Val_int(3));
  Store_field(arr, 1, Val_int(2));
  Store_field(arr, 2, Val_int(1));
	caml_callback(fun2, arr);

	CAMLreturn(Val_unit);
}



CAMLprim value cCudd_Init(value unit)
{
  DdManager* man;
  man = Cudd_Init (0, 0, CUDD_UNIQUE_SLOTS, CUDD_CACHE_SLOTS, 0);
  return (value) man;
}

CAMLprim value cCudd_bddNewVar(value man_ml)
{
  DdManager* man = (DdManager*) man_ml;
  DdNode* node;
  node = Cudd_bddNewVar(man);
  return (value) node;
}

CAMLprim value cCudd_Ref(value node_ml)
{
  DdNode* node = (DdNode*) node_ml;
  Cudd_Ref(node);
  return Val_unit;
}

CAMLprim value cCudd_RecursiveDeref(value man_ml, value node_ml)
{
  DdManager* man = (DdManager*) man_ml;
  DdNode* node = (DdNode*) node_ml;
  Cudd_RecursiveDeref(man, node);
  return Val_unit;
}

CAMLprim value cCudd_bddOr(value man_ml, value node1_ml, value node2_ml)
{
  DdManager* man = (DdManager*) man_ml;
  DdNode* node1 = (DdNode*) node1_ml;
  DdNode* node2 = (DdNode*) node2_ml;
  DdNode* node;
  node = Cudd_bddOr(man, node1, node2);
  return (value) node;
}

CAMLprim value cCudd_bddAnd(value man_ml, value node1_ml, value node2_ml)
{
  DdManager* man = (DdManager*) man_ml;
  DdNode* node1 = (DdNode*) node1_ml;
  DdNode* node2 = (DdNode*) node2_ml;
  DdNode* node;
  node = Cudd_bddAnd(man, node1, node2);
  return (value) node;
}

CAMLprim value cCudd_ReadLogicZero(value man_ml) 
{
  DdManager* man = (DdManager*) man_ml;
  return (value) Cudd_ReadLogicZero(man);
}

CAMLprim value cCudd_ReadOne(value man_ml) 
{
  DdManager* man = (DdManager*) man_ml;
  return (value) Cudd_ReadOne(man);
}


CAMLprim value cCudd_Not(value node) 
{
  return (value) Cudd_Not(node);
}

CAMLprim value cCudd_DagSize(value node_ml) 
{
	CAMLparam1(node_ml);
	DdNode* node = (DdNode*) node_ml;
	CAMLreturn(Val_int(Cudd_DagSize(node)));
}


CAMLprim value cCudd_ForeachCube(value fun, value man_ml, value node_ml) 
{
  CAMLparam3(fun, man_ml, node_ml);
  CAMLlocal1(arr);

  DdManager* man = (DdManager*) man_ml;
  DdNode* node = (DdNode*) node_ml;
  DdGen* gen;
  int* cube;
  int i;
  CUDD_VALUE_TYPE val;

  arr = caml_alloc (Cudd_ReadSize(man), 0);

  Cudd_ForeachCube(man, node, gen, cube, val) 
  {
	for (i = 0; i < Cudd_ReadSize(man); i++)
	{
		Store_field(arr, i, Val_int(cube[i]));
	}
	caml_callback(fun, arr);
  }
  CAMLreturn(Val_unit);
}


CAMLprim value cCudd_ForeachPrime(value fun, value man_ml, value node_ml) 
{
  CAMLparam3(fun, man_ml, node_ml);
  CAMLlocal1(arr);

  DdManager* man = (DdManager*) man_ml;
  DdNode* node = (DdNode*) node_ml;
  DdGen* gen;
  int* cube;
  int i;
  CUDD_VALUE_TYPE val;

  arr = caml_alloc (Cudd_ReadSize(man), 0);

  Cudd_ForeachPrime(man, node, node, gen, cube) 
  {
	for (i = 0; i < Cudd_ReadSize(man); i++)
	{
		Store_field(arr, i, Val_int(cube[i]));
	}
	caml_callback(fun, arr);
  }
  CAMLreturn(Val_unit);
}








/*
value cCudd_DumpDot(value man, value node, value fname) 
{
  CAMLparam3(man, node, fname);
	FILE* fp = fopen(String_val(fname),"w");
  //Cudd_DumpDot(man, 1, node, NULL, NULL, fp);
  fclose(fp);
	CAMLreturn(Val_unit);
}
*/

/*
int main()
{
	printf("hello, world\n");
	return 0;
}
*/

