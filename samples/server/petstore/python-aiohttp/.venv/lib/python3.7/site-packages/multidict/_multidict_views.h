#ifndef _MULTIDICT_VIEWS_H
#define _MULTIDICT_VIEWS_H

#ifdef __cplusplus
extern "C" {
#endif

#include "Python.h"

PyObject *multidict_itemsview_new(PyObject *impl);
PyObject *multidict_keysview_new(PyObject *impl);
PyObject *multidict_valuesview_new(PyObject *impl);

int multidict_views_init();

#ifdef __cplusplus
}
#endif

#endif
