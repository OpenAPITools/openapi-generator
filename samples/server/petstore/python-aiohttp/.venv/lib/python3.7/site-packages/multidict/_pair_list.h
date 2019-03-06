#ifndef _PAIR_LIST_H
#define _PAIR_LIST_H

#ifdef __cplusplus
extern "C" {
#endif

#include <stddef.h>
#include <stdint.h>
#include "Python.h"

PyObject* pair_list_new(void);
PyObject* ci_pair_list_new(void);

Py_ssize_t pair_list_len(PyObject *list);

int pair_list_clear(PyObject *list);

int pair_list_add(PyObject *list, PyObject *key, PyObject *value);

int _pair_list_next(PyObject *list, Py_ssize_t *ppos,
                    PyObject **pidentity,
                    PyObject **pkey, PyObject **pvalue, Py_hash_t *hash);

int pair_list_next(PyObject *list, Py_ssize_t *ppos,
                   PyObject **pidentity,
                   PyObject **pkey, PyObject **pvalue);

int pair_list_contains(PyObject *list, PyObject *key);

PyObject* pair_list_get_one(PyObject *list, PyObject *key);
PyObject* pair_list_get_all(PyObject *list, PyObject *key);

int pair_list_del(PyObject *list, PyObject *key);

PyObject* pair_list_set_default(PyObject *list, PyObject *key, PyObject *value);

PyObject* pair_list_pop_one(PyObject *list, PyObject *key);
PyObject* pair_list_pop_all(PyObject *list, PyObject *key);
PyObject* pair_list_pop_item(PyObject *list);

int pair_list_replace(PyObject *op, PyObject *key, PyObject *value);

int pair_list_update(PyObject *op1, PyObject *op2);
int pair_list_update_from_seq(PyObject *op1, PyObject *op2);

int pair_list_eq_to_mapping(PyObject *op, PyObject *other);

uint64_t pair_list_version(PyObject *list);

int pair_list_init(PyObject * istr_type);

#ifdef __cplusplus
}
#endif
#endif
