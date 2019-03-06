#include "_pair_list.h"

#include <Python.h>

// fix for VisualC complier used by Python 3.4
#ifdef __GNUC__
#define INLINE inline
#else
#define INLINE
#endif

static PyTypeObject multidict_items_iter_type;
static PyTypeObject multidict_values_iter_type;
static PyTypeObject multidict_keys_iter_type;

typedef struct multidict_iter {
    PyObject_HEAD
    PyObject *impl;
    Py_ssize_t current;
    uint64_t version;
} MultidictIter;

static INLINE void
_init_iter(MultidictIter *it, PyObject *impl)
{
    Py_INCREF(impl);

    it->impl = impl;
    it->current = 0;
    it->version = pair_list_version(impl);
}

PyObject *
multidict_items_iter_new(PyObject *impl)
{
    MultidictIter *it = PyObject_GC_New(
        MultidictIter, &multidict_items_iter_type);
    if (it == NULL) {
        return NULL;
    }

    _init_iter(it, impl);

    return (PyObject *)it;
}

PyObject *
multidict_keys_iter_new(PyObject *impl)
{
    MultidictIter *it = PyObject_GC_New(
        MultidictIter, &multidict_keys_iter_type);
    if (it == NULL) {
        return NULL;
    }

    _init_iter(it, impl);

    return (PyObject *)it;
}

PyObject *
multidict_values_iter_new(PyObject *impl)
{
    MultidictIter *it = PyObject_GC_New(
        MultidictIter, &multidict_values_iter_type);
    if (it == NULL) {
        return NULL;
    }

    _init_iter(it, impl);

    return (PyObject *)it;
}

static PyObject *
multidict_items_iter_iternext(MultidictIter *self)
{
    PyObject *key = NULL;
    PyObject *value = NULL;
    PyObject *ret = NULL;

    if (self->version != pair_list_version(self->impl)) {
        PyErr_SetString(PyExc_RuntimeError, "Dictionary changed during iteration");
        return NULL;
    }

    if (!_pair_list_next(self->impl, &self->current, NULL, &key, &value, NULL)) {
        PyErr_SetNone(PyExc_StopIteration);
        return NULL;
    }

    ret = PyTuple_Pack(2, key, value);
    if (ret == NULL) {
        return NULL;
    }

    return ret;
}

static PyObject *
multidict_values_iter_iternext(MultidictIter *self)
{
    PyObject *value = NULL;

    if (self->version != pair_list_version(self->impl)) {
        PyErr_SetString(PyExc_RuntimeError, "Dictionary changed during iteration");
        return NULL;
    }

    if (!pair_list_next(self->impl, &self->current, NULL, NULL, &value)) {
        PyErr_SetNone(PyExc_StopIteration);
        return NULL;
    }

    Py_INCREF(value);

    return value;
}

static PyObject *
multidict_keys_iter_iternext(MultidictIter *self)
{
    PyObject *key = NULL;

    if (self->version != pair_list_version(self->impl)) {
        PyErr_SetString(PyExc_RuntimeError, "Dictionary changed during iteration");
        return NULL;
    }

    if (!pair_list_next(self->impl, &self->current, NULL, &key, NULL)) {
        PyErr_SetNone(PyExc_StopIteration);
        return NULL;
    }

    Py_INCREF(key);

    return key;
}

static void
multidict_iter_dealloc(MultidictIter *self)
{
    PyObject_GC_UnTrack(self);
    Py_XDECREF(self->impl);
    PyObject_GC_Del(self);
}

static int
multidict_iter_traverse(MultidictIter *self, visitproc visit, void *arg)
{
    Py_VISIT(self->impl);
    return 0;
}

static int
multidict_iter_clear(MultidictIter *self)
{
    Py_CLEAR(self->impl);
    return 0;
}

/***********************************************************************/

/* We link this module statically for convenience.  If compiled as a shared
   library instead, some compilers don't allow addresses of Python objects
   defined in other libraries to be used in static initializers here.  The
   DEFERRED_ADDRESS macro is used to tag the slots where such addresses
   appear; the module init function must fill in the tagged slots at runtime.
   The argument is for documentation -- the macro ignores it.
*/
#define DEFERRED_ADDRESS(ADDR) 0

static PyTypeObject multidict_items_iter_type = {
    PyVarObject_HEAD_INIT(DEFERRED_ADDRESS(&PyType_Type), 0)
    "multidict._multidict_iter._itemsiter",         /* tp_name */
    sizeof(MultidictIter),                          /* tp_basicsize */
    0,                                              /* tp_itemsize */
    (destructor)multidict_iter_dealloc,             /* tp_dealloc */
    0,                                              /* tp_print */
    0,                                              /* tp_getattr */
    0,                                              /* tp_setattr */
    0,                                              /* tp_reserved */
    0,                                              /* tp_repr */
    0,                                              /* tp_as_number */
    0,                                              /* tp_as_sequence */
    0,                                              /* tp_as_mapping */
    0,                                              /* tp_hash */
    0,                                              /* tp_call */
    0,                                              /* tp_str */
    0,                                              /* tp_getattro */
    0,                                              /* tp_setattro */
    0,                                              /* tp_as_buffer */
    Py_TPFLAGS_DEFAULT | Py_TPFLAGS_HAVE_GC,        /* tp_flags */
    0,                                              /* tp_doc */
    (traverseproc)multidict_iter_traverse,          /* tp_traverse */
    (inquiry)multidict_iter_clear,                  /* tp_clear */
    0,                                              /* tp_richcompare */
    0,                                              /* tp_weaklistoffset */
    PyObject_SelfIter,                              /* tp_iter */
    (iternextfunc)multidict_items_iter_iternext,    /* tp_iternext */
};

static PyTypeObject multidict_values_iter_type = {
    PyVarObject_HEAD_INIT(DEFERRED_ADDRESS(&PyType_Type), 0)
    "multidict._multidict_iter._valuesiter",         /* tp_name */
    sizeof(MultidictIter),                           /* tp_basicsize */
    0,                                               /* tp_itemsize */
    (destructor)multidict_iter_dealloc,              /* tp_dealloc */
    0,                                               /* tp_print */
    0,                                               /* tp_getattr */
    0,                                               /* tp_setattr */
    0,                                               /* tp_reserved */
    0,                                               /* tp_repr */
    0,                                               /* tp_as_number */
    0,                                               /* tp_as_sequence */
    0,                                               /* tp_as_mapping */
    0,                                               /* tp_hash */
    0,                                               /* tp_call */
    0,                                               /* tp_str */
    0,                                               /* tp_getattro */
    0,                                               /* tp_setattro */
    0,                                               /* tp_as_buffer */
    Py_TPFLAGS_DEFAULT | Py_TPFLAGS_HAVE_GC,         /* tp_flags */
    0,                                               /* tp_doc */
    (traverseproc)multidict_iter_traverse,           /* tp_traverse */
    (inquiry)multidict_iter_clear,                   /* tp_clear */
    0,                                               /* tp_richcompare */
    0,                                               /* tp_weaklistoffset */
    PyObject_SelfIter,                               /* tp_iter */
    (iternextfunc)multidict_values_iter_iternext,    /* tp_iternext */
};

static PyTypeObject multidict_keys_iter_type = {
    PyVarObject_HEAD_INIT(DEFERRED_ADDRESS(&PyType_Type), 0)
    "multidict._multidict_iter._keysiter",         /* tp_name */
    sizeof(MultidictIter),                         /* tp_basicsize */
    0,                                             /* tp_itemsize */
    (destructor)multidict_iter_dealloc,            /* tp_dealloc */
    0,                                             /* tp_print */
    0,                                             /* tp_getattr */
    0,                                             /* tp_setattr */
    0,                                             /* tp_reserved */
    0,                                             /* tp_repr */
    0,                                             /* tp_as_number */
    0,                                             /* tp_as_sequence */
    0,                                             /* tp_as_mapping */
    0,                                             /* tp_hash */
    0,                                             /* tp_call */
    0,                                             /* tp_str */
    0,                                             /* tp_getattro */
    0,                                             /* tp_setattro */
    0,                                             /* tp_as_buffer */
    Py_TPFLAGS_DEFAULT | Py_TPFLAGS_HAVE_GC,       /* tp_flags */
    0,                                             /* tp_doc */
    (traverseproc)multidict_iter_traverse,         /* tp_traverse */
    (inquiry)multidict_iter_clear,                 /* tp_clear */
    0,                                             /* tp_richcompare */
    0,                                             /* tp_weaklistoffset */
    PyObject_SelfIter,                             /* tp_iter */
    (iternextfunc)multidict_keys_iter_iternext,    /* tp_iternext */
};

int
multidict_iter_init()
{
    if (PyType_Ready(&multidict_items_iter_type) < 0 ||
        PyType_Ready(&multidict_values_iter_type) < 0 ||
        PyType_Ready(&multidict_keys_iter_type) < 0) {
        return -1;
    }
    return 0;
}
