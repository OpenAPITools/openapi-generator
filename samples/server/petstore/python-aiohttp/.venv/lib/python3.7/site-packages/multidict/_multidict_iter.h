#ifndef _MULTIDICT_ITER_H
#define _MULTIDICT_ITER_H

#ifdef __cplusplus
extern "C" {
#endif

#include "Python.h"

PyObject *multidict_items_iter_new(PyObject *impl);
PyObject *multidict_keys_iter_new(PyObject *impl);
PyObject *multidict_values_iter_new(PyObject *impl);

int multidict_iter_init();

#ifdef __cplusplus
}
#endif

#endif
