#include "Python.h"
#include "structmember.h"

PyDoc_STRVAR(istr__doc__, "istr class implementation");

/* We link this module statically for convenience.  If compiled as a shared
   library instead, some compilers don't allow addresses of Python objects
   defined in other libraries to be used in static initializers here.  The
   DEFERRED_ADDRESS macro is used to tag the slots where such addresses
   appear; the module init function must fill in the tagged slots at runtime.
   The argument is for documentation -- the macro ignores it.
*/
#define DEFERRED_ADDRESS(ADDR) 0


typedef struct {
    PyUnicodeObject str;
    PyObject * canonical;
} istrobject;

typedef struct {
    PyObject *title;
    PyObject *emptystr;
    PyObject *emptydict;
} ModData;

static struct PyModuleDef _istrmodule;
static PyTypeObject istr_type;

static ModData * 
modstate(PyObject *mod)
{
    return (ModData*)PyModule_GetState(mod);
}

static ModData * 
global_state(void)
{
    return modstate(PyState_FindModule(&_istrmodule));
}



static PyObject *
istr_title(istrobject *self, PyObject *args)
{
    if (!PyArg_ParseTuple(args, ":title"))
        return NULL;
    Py_INCREF(self);
    return (PyObject*)self;
}

static PyObject *
istr_str(istrobject *self)
{
    Py_INCREF(self->canonical);
    return self->canonical;
}

static PyMethodDef istr_methods[] = {
    {"title", (PyCFunction)istr_title, METH_VARARGS,
        PyDoc_STR("title()")},
    {NULL,      NULL},
};


void istr_dealloc(istrobject *self)
{
    Py_XDECREF(self->canonical);
    PyUnicode_Type.tp_dealloc((PyObject*)self);
}

static PyObject *
istr_new(PyTypeObject *type, PyObject *args, PyObject *kwds)
{
    PyObject *x = NULL;
    static char *kwlist[] = {"object", "encoding", "errors", 0};
    char *encoding = NULL;
    char *errors = NULL;
    PyObject *s = NULL;
    PyObject *tmp = NULL;
    PyObject * new_args = NULL;
    PyObject * ret = NULL;

    ModData * state = global_state();

    if (!PyArg_ParseTupleAndKeywords(args, kwds, "|Oss:str",
                                     kwlist, &x, &encoding, &errors))
        return NULL;
    if (x == NULL) {
        s = state->emptystr;
        Py_INCREF(s);
    }
    else if (PyObject_IsInstance(x, (PyObject*)&istr_type)) {
        Py_INCREF(x);
        return x;
    }
    else {
        if (encoding == NULL && errors == NULL) {
            tmp = PyObject_Str(x);
        } else {
            tmp = PyUnicode_FromEncodedObject(x, encoding, errors);
        }
        if (!tmp) {
            goto finish;
        }
        s = PyObject_CallMethodObjArgs(tmp, state->title, NULL);
    }
    if (!s)
        goto finish;

    new_args = PyTuple_Pack(1, s);
    if (!new_args) {
        goto finish;
    }
    ret = PyUnicode_Type.tp_new(type, new_args, state->emptydict);
    if (!ret) {
        goto finish;
    }
    ((istrobject*)ret)->canonical = s;
    s = NULL;  /* the reference is stollen by .canonical */
finish:
    Py_XDECREF(tmp);
    Py_XDECREF(s);
    Py_XDECREF(new_args);
    return ret;
}

static PyTypeObject istr_type = {
    PyVarObject_HEAD_INIT(DEFERRED_ADDRESS(&PyType_Type), 0)
    "multidict._istr.istr",
    sizeof(istrobject),
    0,
    (destructor)istr_dealloc,                   /* tp_dealloc */
    0,                                          /* tp_print */
    0,                                          /* tp_getattr */
    0,                                          /* tp_setattr */
    0,                                          /* tp_reserved */
    0,                                          /* tp_repr */
    0,                                          /* tp_as_number */
    0,                                          /* tp_as_sequence */
    0,                                          /* tp_as_mapping */
    0,                                          /* tp_hash */
    0,                                          /* tp_call */
    (reprfunc)istr_str,                         /* tp_str */
    0,                                          /* tp_getattro */
    0,                                          /* tp_setattro */
    0,                                          /* tp_as_buffer */
    Py_TPFLAGS_DEFAULT | Py_TPFLAGS_BASETYPE | Py_TPFLAGS_UNICODE_SUBCLASS,
                                                /* tp_flags */
    0,                                          /* tp_doc */
    0,                                          /* tp_traverse */
    0,                                          /* tp_clear */
    0,                                          /* tp_richcompare */
    0,                                          /* tp_weaklistoffset */
    0,                                          /* tp_iter */
    0,                                          /* tp_iternext */
    istr_methods,                               /* tp_methods */
    0,                                          /* tp_members */
    0,                                          /* tp_getset */
    DEFERRED_ADDRESS(&PyUnicode_Type),          /* tp_base */
    0,                                          /* tp_dict */
    0,                                          /* tp_descr_get */
    0,                                          /* tp_descr_set */
    0,                                          /* tp_dictoffset */
    0,                                          /* tp_init */
    0,                                          /* tp_alloc */
    (newfunc)istr_new,                          /* tp_new */
};


static int mod_clear(PyObject *m)
{
  Py_CLEAR(modstate(m)->title);
  Py_CLEAR(modstate(m)->emptystr);
  Py_CLEAR(modstate(m)->emptydict);
  return 0;
}


static struct PyModuleDef _istrmodule = {
    PyModuleDef_HEAD_INIT,
    "multidict._istr",
    istr__doc__,
    sizeof(ModData),
    NULL,  /* m_methods */
    NULL,  /* m_reload */
    NULL,  /* m_traverse */
    mod_clear,  /* m_clear */
    NULL   /* m_free */
};


PyObject* PyInit__istr(void)
{
    PyObject * tmp;
    PyObject *mod;

    mod = PyState_FindModule(&_istrmodule);
    if (mod) {
        Py_INCREF(mod);
        return mod;
    }

    istr_type.tp_base = &PyUnicode_Type;
    if (PyType_Ready(&istr_type) < 0) {
        return NULL;
    }

    mod = PyModule_Create(&_istrmodule);
    if (!mod) {
        return NULL;
    }
    tmp = PyUnicode_FromString("title");
    if (!tmp) {
        goto err;
    }
    modstate(mod)->title = tmp;
    tmp = PyUnicode_New(0, 0);
    if (!tmp) {
        goto err;
    }
    modstate(mod)->emptystr = tmp;
    tmp = PyUnicode_FromString("title");
    if(!tmp) {
        goto err;
    }
    modstate(mod)->title = tmp;

    Py_INCREF(&istr_type);
    if (PyModule_AddObject(mod, "istr", (PyObject *)&istr_type) < 0)
        goto err;

    return mod;
err:
    Py_DECREF(mod);
    return NULL;
}
