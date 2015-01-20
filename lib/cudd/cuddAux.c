#include <assert.h>
#include "cuddAux.h"

/* Function Prototypes *
DdNode *cuddauxIsVarInRecur(DdManager* manager, DdNode* f, DdNode* Var);

/**Function********************************************************************

  Synopsis    [Membership of a variable to the support of a BDD/ADD]

  Description [Tells wether a variable appear in the decision diagram
  of a function.]

  SideEffects [None]

  SeeAlso     []

******************************************************************************
int
Cuddaux_IsVarIn(DdManager* dd, DdNode* f, DdNode* var)
{
  assert(Cudd_Regular(var));
  return (cuddauxIsVarInRecur(dd,f,var) == DD_ONE(dd));
}

/**Function********************************************************************

  Synopsis    [Performs the recursive step of Cuddaux_IsVarIn.]

  Description [Performs the recursive step of Cuddaux_IsVarIn. var is
  supposed to be a BDD projection function. Returns the logical one or
  zero.]

  SideEffects [None]

  SeeAlso     []

******************************************************************************
DdNode*
cuddauxIsVarInRecur(DdManager* manager, DdNode* f, DdNode* Var)
{
  DdNode *zero,*one, *F, *res;
  int topV,topF;

  one = DD_ONE(manager);
  zero = Cudd_Not(one);
  F = Cudd_Regular(f);

  if (cuddIsConstant(F)) return zero;
  if (Var==F) return(one);

  topV = Var->index;
  topF = F->index;
  if (topF == topV) return(one);
  if (cuddI(manager,topV) < cuddI(manager,topF)) return(zero);

  res = cuddCacheLookup2(manager,cuddauxIsVarInRecur, F, Var);
  if (res != NULL) return(res);
  res = cuddauxIsVarInRecur(manager,cuddT(F),Var);
  if (res==zero){
    res = cuddauxIsVarInRecur(manager,cuddE(F),Var);
  }
  cuddCacheInsert2(manager,cuddauxIsVarInRecur,F,Var,res);
  return(res);
}
*/
