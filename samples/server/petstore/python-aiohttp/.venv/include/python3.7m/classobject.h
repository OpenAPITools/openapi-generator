/* Former class object interface -- now only bound methods are here  */

/* Revealing some structures (not for general use) */

#ifndef Py_LIMITED_API
#ifndef Py_CLASSOBJECT_H
#define Py_CLASSOBJECT_H
#ifdef __cplusplus
extern "C" {
#endif

typedef struct {
    PyObject_HEAD
    PyObject *im_func;   /* The callable object implementing the method */
    PyObject *im_self;   /* The instance it is bound to */
    PyObject *im_weakreflist; /* List of weak references */
} PyMethodObject;

PyAPI_DATA(PyTypeObject) PyMethod_Type;

#define PyMethod_Check(op) ((op)->ob_type == &PyMethod_Type)

PyAPI_FUNC(PyObject *) PyMethod_New(PyObject *, PyObject *);

PyAPI_FUNC(PyObject *) PyMethod_Function(PyObject *);
PyAPI_FUNC(PyObject *) PyMethod_Self(PyObject *);

/* Macros for direct access to these values. Type checks are *not*
   done, so use with care. */
#define PyMethod_GET_FUNCTION(meth) \
        (((PyMethodObject *)meth) -> im_func)
#define PyMethod_GET_SELF(meth) \
        (((PyMethodObject *)meth) -> im_self)

PyAPI_FUNC(int) PyMethod_ClearFreeList(void);

typedef struct {
    PyObject_HEAD
    PyObject *func;
} PyInstanceMethodObject;

PyAPI_DATA(PyTypeObject) PyInstanceMethod_Type;

#define PyInstanceMethod_Check(op) ((op)->ob_type == &PyInstanceMethod_Type)

PyAPI_FUNC(PyObject *) PyInstanceMethod_New(PyObject *);
PyAPI_FUNC(PyObject *) PyInstanceMethod_Function(PyObject *);

/* Macros for direct access to these values. Type checks are *not*
   done, so use with care. */
#define PyInstanceMethod_GET_FUNCTION(meth) \
        (((PyInstanceMethodObject *)meth) -> func)

#ifdef __cplusplus
}
#endif
#endif /* !Py_CLASSOBJECT_H */
#endif /* Py_LIMITED_API */
