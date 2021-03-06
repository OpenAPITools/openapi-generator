// List primitive operations
//
// These are registered in mypyc.primitives.list_ops.

#include <Python.h>
#include "CPy.h"

PyObject *CPyList_GetItemUnsafe(PyObject *list, CPyTagged index) {
    Py_ssize_t n = CPyTagged_ShortAsSsize_t(index);
    PyObject *result = PyList_GET_ITEM(list, n);
    Py_INCREF(result);
    return result;
}

PyObject *CPyList_GetItemShort(PyObject *list, CPyTagged index) {
    Py_ssize_t n = CPyTagged_ShortAsSsize_t(index);
    Py_ssize_t size = PyList_GET_SIZE(list);
    if (n >= 0) {
        if (n >= size) {
            PyErr_SetString(PyExc_IndexError, "list index out of range");
            return NULL;
        }
    } else {
        n += size;
        if (n < 0) {
            PyErr_SetString(PyExc_IndexError, "list index out of range");
            return NULL;
        }
    }
    PyObject *result = PyList_GET_ITEM(list, n);
    Py_INCREF(result);
    return result;
}

PyObject *CPyList_GetItem(PyObject *list, CPyTagged index) {
    if (CPyTagged_CheckShort(index)) {
        Py_ssize_t n = CPyTagged_ShortAsSsize_t(index);
        Py_ssize_t size = PyList_GET_SIZE(list);
        if (n >= 0) {
            if (n >= size) {
                PyErr_SetString(PyExc_IndexError, "list index out of range");
                return NULL;
            }
        } else {
            n += size;
            if (n < 0) {
                PyErr_SetString(PyExc_IndexError, "list index out of range");
                return NULL;
            }
        }
        PyObject *result = PyList_GET_ITEM(list, n);
        Py_INCREF(result);
        return result;
    } else {
        PyErr_SetString(PyExc_IndexError, "list index out of range");
        return NULL;
    }
}

bool CPyList_SetItem(PyObject *list, CPyTagged index, PyObject *value) {
    if (CPyTagged_CheckShort(index)) {
        Py_ssize_t n = CPyTagged_ShortAsSsize_t(index);
        Py_ssize_t size = PyList_GET_SIZE(list);
        if (n >= 0) {
            if (n >= size) {
                PyErr_SetString(PyExc_IndexError, "list assignment index out of range");
                return false;
            }
        } else {
            n += size;
            if (n < 0) {
                PyErr_SetString(PyExc_IndexError, "list assignment index out of range");
                return false;
            }
        }
        // PyList_SET_ITEM doesn't decref the old element, so we do
        Py_DECREF(PyList_GET_ITEM(list, n));
        // N.B: Steals reference
        PyList_SET_ITEM(list, n, value);
        return true;
    } else {
        PyErr_SetString(PyExc_IndexError, "list assignment index out of range");
        return false;
    }
}

PyObject *CPyList_PopLast(PyObject *obj)
{
    // I tried a specalized version of pop_impl for just removing the
    // last element and it wasn't any faster in microbenchmarks than
    // the generic one so I ditched it.
    return list_pop_impl((PyListObject *)obj, -1);
}

PyObject *CPyList_Pop(PyObject *obj, CPyTagged index)
{
    if (CPyTagged_CheckShort(index)) {
        Py_ssize_t n = CPyTagged_ShortAsSsize_t(index);
        return list_pop_impl((PyListObject *)obj, n);
    } else {
        PyErr_SetString(PyExc_IndexError, "pop index out of range");
        return NULL;
    }
}

CPyTagged CPyList_Count(PyObject *obj, PyObject *value)
{
    return list_count((PyListObject *)obj, value);
}

int CPyList_Insert(PyObject *list, CPyTagged index, PyObject *value)
{
    if (CPyTagged_CheckShort(index)) {
        Py_ssize_t n = CPyTagged_ShortAsSsize_t(index);
        return PyList_Insert(list, n, value);
    }
    return -1;
}

PyObject *CPyList_Extend(PyObject *o1, PyObject *o2) {
    return _PyList_Extend((PyListObject *)o1, o2);
}

PyObject *CPySequence_Multiply(PyObject *seq, CPyTagged t_size) {
    Py_ssize_t size = CPyTagged_AsSsize_t(t_size);
    if (size == -1 && PyErr_Occurred()) {
        return NULL;
    }
    return PySequence_Repeat(seq, size);
}

PyObject *CPySequence_RMultiply(CPyTagged t_size, PyObject *seq) {
    return CPySequence_Multiply(seq, t_size);
}

PyObject *CPyList_GetSlice(PyObject *obj, CPyTagged start, CPyTagged end) {
    if (likely(PyList_CheckExact(obj)
               && CPyTagged_CheckShort(start) && CPyTagged_CheckShort(end))) {
        Py_ssize_t startn = CPyTagged_ShortAsSsize_t(start);
        Py_ssize_t endn = CPyTagged_ShortAsSsize_t(end);
        if (startn < 0) {
            startn += PyList_GET_SIZE(obj);
        }
        if (endn < 0) {
            endn += PyList_GET_SIZE(obj);
        }
        return PyList_GetSlice(obj, startn, endn);
    }
    return CPyObject_GetSlice(obj, start, end);
}
