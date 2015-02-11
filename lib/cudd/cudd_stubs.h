#ifndef __CUDD_STUBS_H__
#define __CUDD_STUBS_H__

#include <stdio.h>
#include "mlcudd.h"
#include "cuddInt.h"
#include "cudd.h"

typedef node_t bdd_t;

#define Manager_val(v) (*((manager_t**) Data_custom_val(v)))
#define Node_val(b) ((node_t*) Data_custom_val(b))

/* Function prototypes */
CAMLprim value caml_cudd_manager_make(value unit);

#endif
