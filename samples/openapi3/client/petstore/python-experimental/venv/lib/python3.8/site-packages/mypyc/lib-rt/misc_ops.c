// Misc primitive operations
//
// These are registered in mypyc.primitives.misc_ops.

#include <Python.h>
#include "CPy.h"

PyObject *CPy_GetCoro(PyObject *obj)
{
    // If the type has an __await__ method, call it,
    // otherwise, fallback to calling __iter__.
    PyAsyncMethods* async_struct = obj->ob_type->tp_as_async;
    if (async_struct != NULL && async_struct->am_await != NULL) {
        return (async_struct->am_await)(obj);
    } else {
        // TODO: We should check that the type is a generator decorated with
        // asyncio.coroutine
        return PyObject_GetIter(obj);
    }
}

PyObject *CPyIter_Send(PyObject *iter, PyObject *val)
{
    // Do a send, or a next if second arg is None.
    // (This behavior is to match the PEP 380 spec for yield from.)
    _Py_IDENTIFIER(send);
    if (val == Py_None) {
        return CPyIter_Next(iter);
    } else {
        return _PyObject_CallMethodIdObjArgs(iter, &PyId_send, val, NULL);
    }
}

// A somewhat hairy implementation of specifically most of the error handling
// in `yield from` error handling. The point here is to reduce code size.
//
// This implements most of the bodies of the `except` blocks in the
// pseudocode in PEP 380.
//
// Returns true (1) if a StopIteration was received and we should return.
// Returns false (0) if a value should be yielded.
// In both cases the value is stored in outp.
// Signals an error (2) if the an exception should be propagated.
int CPy_YieldFromErrorHandle(PyObject *iter, PyObject **outp)
{
    _Py_IDENTIFIER(close);
    _Py_IDENTIFIER(throw);
    PyObject *exc_type = CPy_ExcState()->exc_type;
    PyObject *type, *value, *traceback;
    PyObject *_m;
    PyObject *res;
    *outp = NULL;

    if (PyErr_GivenExceptionMatches(exc_type, PyExc_GeneratorExit)) {
        _m = _PyObject_GetAttrId(iter, &PyId_close);
        if (_m) {
            res = PyObject_CallFunctionObjArgs(_m, NULL);
            Py_DECREF(_m);
            if (!res)
                return 2;
            Py_DECREF(res);
        } else if (PyErr_ExceptionMatches(PyExc_AttributeError)) {
            PyErr_Clear();
        } else {
            return 2;
        }
    } else {
        _m = _PyObject_GetAttrId(iter, &PyId_throw);
        if (_m) {
            _CPy_GetExcInfo(&type, &value, &traceback);
            res = PyObject_CallFunctionObjArgs(_m, type, value, traceback, NULL);
            Py_DECREF(type);
            Py_DECREF(value);
            Py_DECREF(traceback);
            Py_DECREF(_m);
            if (res) {
                *outp = res;
                return 0;
            } else {
                res = CPy_FetchStopIterationValue();
                if (res) {
                    *outp = res;
                    return 1;
                }
            }
        } else if (PyErr_ExceptionMatches(PyExc_AttributeError)) {
            PyErr_Clear();
        } else {
            return 2;
        }
    }

    CPy_Reraise();
    return 2;
}

PyObject *CPy_FetchStopIterationValue(void)
{
    PyObject *val = NULL;
    _PyGen_FetchStopIterationValue(&val);
    return val;
}

static bool _CPy_IsSafeMetaClass(PyTypeObject *metaclass) {
    // mypyc classes can't work with metaclasses in
    // general. Through some various nasty hacks we *do*
    // manage to work with TypingMeta and its friends.
    if (metaclass == &PyType_Type)
        return true;
    PyObject *module = PyObject_GetAttrString((PyObject *)metaclass, "__module__");
    if (!module) {
        PyErr_Clear();
        return false;
    }

    bool matches = false;
    if (PyUnicode_CompareWithASCIIString(module, "typing") == 0 &&
            (strcmp(metaclass->tp_name, "TypingMeta") == 0
             || strcmp(metaclass->tp_name, "GenericMeta") == 0
             || strcmp(metaclass->tp_name, "_ProtocolMeta") == 0)) {
        matches = true;
    } else if (PyUnicode_CompareWithASCIIString(module, "typing_extensions") == 0 &&
               strcmp(metaclass->tp_name, "_ProtocolMeta") == 0) {
        matches = true;
    } else if (PyUnicode_CompareWithASCIIString(module, "abc") == 0 &&
               strcmp(metaclass->tp_name, "ABCMeta") == 0) {
        matches = true;
    }
    Py_DECREF(module);
    return matches;
}

// Create a heap type based on a template non-heap type.
// This is super hacky and maybe we should suck it up and use PyType_FromSpec instead.
// We allow bases to be NULL to represent just inheriting from object.
// We don't support NULL bases and a non-type metaclass.
PyObject *CPyType_FromTemplate(PyObject *template,
                               PyObject *orig_bases,
                               PyObject *modname) {
    PyTypeObject *template_ = (PyTypeObject *)template;
    PyHeapTypeObject *t = NULL;
    PyTypeObject *dummy_class = NULL;
    PyObject *name = NULL;
    PyObject *bases = NULL;
    PyObject *slots;

    // If the type of the class (the metaclass) is NULL, we default it
    // to being type.  (This allows us to avoid needing to initialize
    // it explicitly on windows.)
    if (!Py_TYPE(template_)) {
        Py_TYPE(template_) = &PyType_Type;
    }
    PyTypeObject *metaclass = Py_TYPE(template_);

    if (orig_bases) {
        bases = update_bases(orig_bases);
        // update_bases doesn't increment the refcount if nothing changes,
        // so we do it to make sure we have distinct "references" to both
        if (bases == orig_bases)
            Py_INCREF(bases);

        // Find the appropriate metaclass from our base classes. We
        // care about this because Generic uses a metaclass prior to
        // Python 3.7.
        metaclass = _PyType_CalculateMetaclass(metaclass, bases);
        if (!metaclass)
            goto error;

        if (!_CPy_IsSafeMetaClass(metaclass)) {
            PyErr_SetString(PyExc_TypeError, "mypyc classes can't have a metaclass");
            goto error;
        }
    }

    name = PyUnicode_FromString(template_->tp_name);
    if (!name)
        goto error;

    // If there is a metaclass other than type, we would like to call
    // its __new__ function. Unfortunately there doesn't seem to be a
    // good way to mix a C extension class and creating it via a
    // metaclass. We need to do it anyways, though, in order to
    // support subclassing Generic[T] prior to Python 3.7.
    //
    // We solve this with a kind of atrocious hack: create a parallel
    // class using the metaclass, determine the bases of the real
    // class by pulling them out of the parallel class, creating the
    // real class, and then merging its dict back into the original
    // class. There are lots of cases where this won't really work,
    // but for the case of GenericMeta setting a bunch of properties
    // on the class we should be fine.
    if (metaclass != &PyType_Type) {
        assert(bases && "non-type metaclasses require non-NULL bases");

        PyObject *ns = PyDict_New();
        if (!ns)
            goto error;

        if (bases != orig_bases) {
            if (PyDict_SetItemString(ns, "__orig_bases__", orig_bases) < 0)
                goto error;
        }

        dummy_class = (PyTypeObject *)PyObject_CallFunctionObjArgs(
            (PyObject *)metaclass, name, bases, ns, NULL);
        Py_DECREF(ns);
        if (!dummy_class)
            goto error;

        Py_DECREF(bases);
        bases = dummy_class->tp_bases;
        Py_INCREF(bases);
    }

    // Allocate the type and then copy the main stuff in.
    t = (PyHeapTypeObject*)PyType_GenericAlloc(&PyType_Type, 0);
    if (!t)
        goto error;
    memcpy((char *)t + sizeof(PyVarObject),
           (char *)template_ + sizeof(PyVarObject),
           sizeof(PyTypeObject) - sizeof(PyVarObject));

    if (bases != orig_bases) {
        if (PyObject_SetAttrString((PyObject *)t, "__orig_bases__", orig_bases) < 0)
            goto error;
    }

    // Having tp_base set is I think required for stuff to get
    // inherited in PyType_Ready, which we needed for subclassing
    // BaseException. XXX: Taking the first element is wrong I think though.
    if (bases) {
        t->ht_type.tp_base = (PyTypeObject *)PyTuple_GET_ITEM(bases, 0);
        Py_INCREF((PyObject *)t->ht_type.tp_base);
    }

    t->ht_name = name;
    Py_INCREF(name);
    t->ht_qualname = name;
    t->ht_type.tp_bases = bases;
    // references stolen so NULL these out
    bases = name = NULL;

    if (PyType_Ready((PyTypeObject *)t) < 0)
        goto error;

    assert(t->ht_type.tp_base != NULL);

    // XXX: This is a terrible hack to work around a cpython check on
    // the mro. It was needed for mypy.stats. I need to investigate
    // what is actually going on here.
    Py_INCREF(metaclass);
    Py_TYPE(t) = metaclass;

    if (dummy_class) {
        if (PyDict_Merge(t->ht_type.tp_dict, dummy_class->tp_dict, 0) != 0)
            goto error;
        // This is the *really* tasteless bit. GenericMeta's __new__
        // in certain versions of typing sets _gorg to point back to
        // the class. We need to override it to keep it from pointing
        // to the proxy.
        if (PyDict_SetItemString(t->ht_type.tp_dict, "_gorg", (PyObject *)t) < 0)
            goto error;
    }

    // Reject anything that would give us a nontrivial __slots__,
    // because the layout will conflict
    slots = PyObject_GetAttrString((PyObject *)t, "__slots__");
    if (slots) {
        // don't fail on an empty __slots__
        int is_true = PyObject_IsTrue(slots);
        Py_DECREF(slots);
        if (is_true > 0)
            PyErr_SetString(PyExc_TypeError, "mypyc classes can't have __slots__");
        if (is_true != 0)
            goto error;
    } else {
        PyErr_Clear();
    }

    if (PyObject_SetAttrString((PyObject *)t, "__module__", modname) < 0)
        goto error;

    if (init_subclass((PyTypeObject *)t, NULL))
        goto error;

    Py_XDECREF(dummy_class);

    return (PyObject *)t;

error:
    Py_XDECREF(t);
    Py_XDECREF(bases);
    Py_XDECREF(dummy_class);
    Py_XDECREF(name);
    return NULL;
}

static int _CPy_UpdateObjFromDict(PyObject *obj, PyObject *dict)
{
    Py_ssize_t pos = 0;
    PyObject *key, *value;
    while (PyDict_Next(dict, &pos, &key, &value)) {
        if (PyObject_SetAttr(obj, key, value) != 0) {
            return -1;
        }
    }
    return 0;
}

/* Support for our partial built-in support for dataclasses.
 *
 * Take a class we want to make a dataclass, remove any descriptors
 * for annotated attributes, swap in the actual values of the class
 * variables invoke dataclass, and then restore all of the
 * descriptors.
 *
 * The purpose of all this is that dataclasses uses the values of
 * class variables to drive which attributes are required and what the
 * default values/factories are for optional attributes. This means
 * that the class dict needs to contain those values instead of getset
 * descriptors for the attributes when we invoke dataclass.
 *
 * We need to remove descriptors for attributes even when there is no
 * default value for them, or else dataclass will think the descriptor
 * is the default value. We remove only the attributes, since we don't
 * want dataclasses to try generating functions when they are already
 * implemented.
 *
 * Args:
 *   dataclass_dec: The decorator to apply
 *   tp: The class we are making a dataclass
 *   dict: The dictionary containing values that dataclasses needs
 *   annotations: The type annotation dictionary
 */
int
CPyDataclass_SleightOfHand(PyObject *dataclass_dec, PyObject *tp,
                           PyObject *dict, PyObject *annotations) {
    PyTypeObject *ttp = (PyTypeObject *)tp;
    Py_ssize_t pos;
    PyObject *res;

    /* Make a copy of the original class __dict__ */
    PyObject *orig_dict = PyDict_Copy(ttp->tp_dict);
    if (!orig_dict) {
        goto fail;
    }

    /* Delete anything that had an annotation */
    pos = 0;
    PyObject *key;
    while (PyDict_Next(annotations, &pos, &key, NULL)) {
        if (PyObject_DelAttr(tp, key) != 0) {
            goto fail;
        }
    }

    /* Copy in all the attributes that we want dataclass to see */
    if (_CPy_UpdateObjFromDict(tp, dict) != 0) {
        goto fail;
    }

    /* Run the @dataclass descriptor */
    res = PyObject_CallFunctionObjArgs(dataclass_dec, tp, NULL);
    if (!res) {
        goto fail;
    }
    Py_DECREF(res);

    /* Copy back the original contents of the dict */
    if (_CPy_UpdateObjFromDict(tp, orig_dict) != 0) {
        goto fail;
    }

    Py_DECREF(orig_dict);
    return 1;

fail:
    Py_XDECREF(orig_dict);
    return 0;
}

// Support for pickling; reusable getstate and setstate functions
PyObject *
CPyPickle_SetState(PyObject *obj, PyObject *state)
{
    if (_CPy_UpdateObjFromDict(obj, state) != 0) {
        return NULL;
    }
    Py_RETURN_NONE;
}

PyObject *
CPyPickle_GetState(PyObject *obj)
{
    PyObject *attrs = NULL, *state = NULL;

    attrs = PyObject_GetAttrString((PyObject *)Py_TYPE(obj), "__mypyc_attrs__");
    if (!attrs) {
        goto fail;
    }
    if (!PyTuple_Check(attrs)) {
        PyErr_SetString(PyExc_TypeError, "__mypyc_attrs__ is not a tuple");
        goto fail;
    }
    state = PyDict_New();
    if (!state) {
        goto fail;
    }

    // Collect all the values of attributes in __mypyc_attrs__
    // Attributes that are missing we just ignore
    int i;
    for (i = 0; i < PyTuple_GET_SIZE(attrs); i++) {
        PyObject *key = PyTuple_GET_ITEM(attrs, i);
        PyObject *value = PyObject_GetAttr(obj, key);
        if (!value) {
            if (PyErr_ExceptionMatches(PyExc_AttributeError)) {
                PyErr_Clear();
                continue;
            }
            goto fail;
        }
        int result = PyDict_SetItem(state, key, value);
        Py_DECREF(value);
        if (result != 0) {
            goto fail;
        }
    }

    Py_DECREF(attrs);

    return state;
fail:
    Py_XDECREF(attrs);
    Py_XDECREF(state);
    return NULL;
}

CPyTagged CPyTagged_Id(PyObject *o) {
    return CPyTagged_FromSsize_t((Py_ssize_t)o);
}

#define MAX_INT_CHARS 22
#define _PyUnicode_LENGTH(op)                           \
    (((PyASCIIObject *)(op))->length)

// using snprintf or PyUnicode_FromFormat was way slower than
// boxing the int and calling PyObject_Str on it, so we implement our own
static int fmt_ssize_t(char *out, Py_ssize_t n) {
	bool neg = n < 0;
	if (neg) n = -n;

	// buf gets filled backward and then we copy it forward
	char buf[MAX_INT_CHARS];
	int i = 0;
	do {
		buf[i] = (n % 10) + '0';
		n /= 10;
		i++;
	} while (n);


	int len = i;
	int j = 0;
	if (neg) {
		out[j++] = '-';
		len++;
	}

	for (; j < len; j++, i--) {
		out[j] = buf[i-1];
	}
	out[j] = '\0';

	return len;
}

static PyObject *CPyTagged_ShortToStr(Py_ssize_t n) {
    PyObject *obj = PyUnicode_New(MAX_INT_CHARS, 127);
    if (!obj) return NULL;
    int len = fmt_ssize_t((char *)PyUnicode_1BYTE_DATA(obj), n);
    _PyUnicode_LENGTH(obj) = len;
    return obj;
}

PyObject *CPyTagged_Str(CPyTagged n) {
    if (CPyTagged_CheckShort(n)) {
        return CPyTagged_ShortToStr(CPyTagged_ShortAsSsize_t(n));
    } else {
        return PyObject_Str(CPyTagged_AsObject(n));
    }
}

void CPyDebug_Print(const char *msg) {
    printf("%s\n", msg);
    fflush(stdout);
}

int CPySequence_CheckUnpackCount(PyObject *sequence, Py_ssize_t expected) {
    Py_ssize_t actual = Py_SIZE(sequence);
    if (unlikely(actual != expected)) {
        if (actual < expected) {
            PyErr_Format(PyExc_ValueError, "not enough values to unpack (expected %zd, got %zd)",
                         expected, actual);
        } else {
            PyErr_Format(PyExc_ValueError, "too many values to unpack (expected %zd)", expected);
        }
        return -1;
    }
    return 0;
}
