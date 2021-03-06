// String primitive operations
//
// These are registered in mypyc.primitives.str_ops.

#include <Python.h>
#include "CPy.h"

PyObject *CPyStr_GetItem(PyObject *str, CPyTagged index) {
    if (PyUnicode_READY(str) != -1) {
        if (CPyTagged_CheckShort(index)) {
            Py_ssize_t n = CPyTagged_ShortAsSsize_t(index);
            Py_ssize_t size = PyUnicode_GET_LENGTH(str);
            if ((n >= 0 && n >= size) || (n < 0 && n + size < 0)) {
                PyErr_SetString(PyExc_IndexError, "string index out of range");
                return NULL;
            }
            if (n < 0)
                n += size;
            enum PyUnicode_Kind kind = (enum PyUnicode_Kind)PyUnicode_KIND(str);
            void *data = PyUnicode_DATA(str);
            Py_UCS4 ch = PyUnicode_READ(kind, data, n);
            PyObject *unicode = PyUnicode_New(1, ch);
            if (unicode == NULL)
                return NULL;

            if (PyUnicode_KIND(unicode) == PyUnicode_1BYTE_KIND) {
                PyUnicode_1BYTE_DATA(unicode)[0] = (Py_UCS1)ch;
            }
            else if (PyUnicode_KIND(unicode) == PyUnicode_2BYTE_KIND) {
                PyUnicode_2BYTE_DATA(unicode)[0] = (Py_UCS2)ch;
            } else {
                assert(PyUnicode_KIND(unicode) == PyUnicode_4BYTE_KIND);
                PyUnicode_4BYTE_DATA(unicode)[0] = ch;
            }
            return unicode;
        } else {
            PyErr_SetString(PyExc_IndexError, "string index out of range");
            return NULL;
        }
    } else {
        PyObject *index_obj = CPyTagged_AsObject(index);
        return PyObject_GetItem(str, index_obj);
    }
}

PyObject *CPyStr_Split(PyObject *str, PyObject *sep, CPyTagged max_split)
{
    Py_ssize_t temp_max_split = CPyTagged_AsSsize_t(max_split);
    if (temp_max_split == -1 && PyErr_Occurred()) {
        PyErr_SetString(PyExc_OverflowError, "Python int too large to convert to C ssize_t");
            return NULL;
    }
    return PyUnicode_Split(str, sep, temp_max_split);
}

bool CPyStr_Startswith(PyObject *self, PyObject *subobj) {
    Py_ssize_t start = 0;
    Py_ssize_t end = PyUnicode_GET_LENGTH(self);
    return PyUnicode_Tailmatch(self, subobj, start, end, -1);
}

bool CPyStr_Endswith(PyObject *self, PyObject *subobj) {
    Py_ssize_t start = 0;
    Py_ssize_t end = PyUnicode_GET_LENGTH(self);
    return PyUnicode_Tailmatch(self, subobj, start, end, 1);
}

/* This does a dodgy attempt to append in place  */
PyObject *CPyStr_Append(PyObject *o1, PyObject *o2) {
    PyUnicode_Append(&o1, o2);
    return o1;
}

PyObject *CPyStr_GetSlice(PyObject *obj, CPyTagged start, CPyTagged end) {
    if (likely(PyUnicode_CheckExact(obj)
               && CPyTagged_CheckShort(start) && CPyTagged_CheckShort(end))) {
        Py_ssize_t startn = CPyTagged_ShortAsSsize_t(start);
        Py_ssize_t endn = CPyTagged_ShortAsSsize_t(end);
        if (startn < 0) {
            startn += PyUnicode_GET_LENGTH(obj);
            if (startn < 0) {
                startn = 0;
            }
        }
        if (endn < 0) {
            endn += PyUnicode_GET_LENGTH(obj);
            if (endn < 0) {
                endn = 0;
            }
        }
        return PyUnicode_Substring(obj, startn, endn);
    }
    return CPyObject_GetSlice(obj, start, end);
}
