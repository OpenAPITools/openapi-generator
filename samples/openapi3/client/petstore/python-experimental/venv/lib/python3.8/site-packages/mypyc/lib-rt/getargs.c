/* getargs implementation copied from Python 3.8 and stripped down to only include
 * the functions we need.
 * We also add support for required kwonly args and accepting *args / **kwargs.
 * A good idea would be to also vendor in the Fast versions and get our stuff
 * working with *that*.
 * Another probably good idea is to strip out all the formatting stuff we don't need
 * and then add in custom stuff that we do need.
 *
 * DOCUMENTATION OF THE EXTENSIONS:
 *  - Arguments given after a @ format specify are required keyword-only arguments.
 *    The | and $ specifiers must both appear before @.
 *  - If the first character of a format string is %, then the function can support
 *    *args and **kwargs. On seeing a %, the parser will consume two arguments,
 *    which should be pointers to variables to store the *args and **kwargs, respectively.
 *    Either pointer can be NULL, in which case the function doesn't take that
 *    variety of vararg.
 *    Unlike most format specifiers, the caller takes ownership of these objects
 *    and is responsible for decrefing them.
 */

// These macro definitions are copied from pyport.h in Python 3.9 and later
// https://bugs.python.org/issue19569
#if defined(__clang__)
#define _Py_COMP_DIAG_PUSH _Pragma("clang diagnostic push")
#define _Py_COMP_DIAG_IGNORE_DEPR_DECLS \
    _Pragma("clang diagnostic ignored \"-Wdeprecated-declarations\"")
#define _Py_COMP_DIAG_POP _Pragma("clang diagnostic pop")
#elif defined(__GNUC__) \
    && ((__GNUC__ >= 5) || (__GNUC__ == 4) && (__GNUC_MINOR__ >= 6))
#define _Py_COMP_DIAG_PUSH _Pragma("GCC diagnostic push")
#define _Py_COMP_DIAG_IGNORE_DEPR_DECLS \
    _Pragma("GCC diagnostic ignored \"-Wdeprecated-declarations\"")
#define _Py_COMP_DIAG_POP _Pragma("GCC diagnostic pop")
#elif defined(_MSC_VER)
#define _Py_COMP_DIAG_PUSH __pragma(warning(push))
#define _Py_COMP_DIAG_IGNORE_DEPR_DECLS __pragma(warning(disable: 4996))
#define _Py_COMP_DIAG_POP __pragma(warning(pop))
#else
#define _Py_COMP_DIAG_PUSH
#define _Py_COMP_DIAG_IGNORE_DEPR_DECLS
#define _Py_COMP_DIAG_POP
#endif

#include "Python.h"
#include "pythonsupport.h"

#include <ctype.h>
#include <float.h>

#define _PyTuple_CAST(op) (assert(PyTuple_Check(op)), (PyTupleObject *)(op))
#define _PyTuple_ITEMS(op) (_PyTuple_CAST(op)->ob_item)
#ifndef PyDict_GET_SIZE
#define PyDict_GET_SIZE(d) PyDict_Size(d)
#endif


#ifdef __cplusplus
extern "C" {
#endif
int CPyArg_ParseTupleAndKeywords(PyObject *, PyObject *,
                                 const char *, char **, ...);
int CPyArg_VaParseTupleAndKeywords(PyObject *, PyObject *,
                                   const char *, char **, va_list);


#define FLAG_COMPAT 1
#define FLAG_SIZE_T 2

typedef int (*destr_t)(PyObject *, void *);


/* Keep track of "objects" that have been allocated or initialized and
   which will need to be deallocated or cleaned up somehow if overall
   parsing fails.
*/
typedef struct {
  void *item;
  destr_t destructor;
} freelistentry_t;

typedef struct {
  freelistentry_t *entries;
  int first_available;
  int entries_malloced;
} freelist_t;

#define STATIC_FREELIST_ENTRIES 8

/* Forward */
static void seterror(Py_ssize_t, const char *, int *, const char *, const char *);
static const char *convertitem(PyObject *, const char **, va_list *, int, int *,
                               char *, size_t, freelist_t *);
static const char *converttuple(PyObject *, const char **, va_list *, int,
                                int *, char *, size_t, int, freelist_t *);
static const char *convertsimple(PyObject *, const char **, va_list *, int,
                                 char *, size_t, freelist_t *);
static Py_ssize_t convertbuffer(PyObject *, const void **p, const char **);
static int getbuffer(PyObject *, Py_buffer *, const char**);

static int vgetargskeywords(PyObject *, PyObject *,
                            const char *, char **, va_list *, int);
static const char *skipitem(const char **, va_list *, int);

/* Handle cleanup of allocated memory in case of exception */

static int
cleanup_ptr(PyObject *self, void *ptr)
{
    if (ptr) {
        PyMem_FREE(ptr);
    }
    return 0;
}

static int
cleanup_buffer(PyObject *self, void *ptr)
{
    Py_buffer *buf = (Py_buffer *)ptr;
    if (buf) {
        PyBuffer_Release(buf);
    }
    return 0;
}

static int
addcleanup(void *ptr, freelist_t *freelist, destr_t destructor)
{
    int index;

    index = freelist->first_available;
    freelist->first_available += 1;

    freelist->entries[index].item = ptr;
    freelist->entries[index].destructor = destructor;

    return 0;
}

static int
cleanreturn(int retval, freelist_t *freelist)
{
    int index;

    if (retval == 0) {
      /* A failure occurred, therefore execute all of the cleanup
         functions.
      */
      for (index = 0; index < freelist->first_available; ++index) {
          freelist->entries[index].destructor(NULL,
                                              freelist->entries[index].item);
      }
    }
    if (freelist->entries_malloced)
        PyMem_FREE(freelist->entries);
    return retval;
}


static void
seterror(Py_ssize_t iarg, const char *msg, int *levels, const char *fname,
         const char *message)
{
    char buf[512];
    int i;
    char *p = buf;

    if (PyErr_Occurred())
        return;
    else if (message == NULL) {
        if (fname != NULL) {
            PyOS_snprintf(p, sizeof(buf), "%.200s() ", fname);
            p += strlen(p);
        }
        if (iarg != 0) {
            PyOS_snprintf(p, sizeof(buf) - (p - buf),
                          "argument %" PY_FORMAT_SIZE_T "d", iarg);
            i = 0;
            p += strlen(p);
            while (i < 32 && levels[i] > 0 && (int)(p-buf) < 220) {
                PyOS_snprintf(p, sizeof(buf) - (p - buf),
                              ", item %d", levels[i]-1);
                p += strlen(p);
                i++;
            }
        }
        else {
            PyOS_snprintf(p, sizeof(buf) - (p - buf), "argument");
            p += strlen(p);
        }
        PyOS_snprintf(p, sizeof(buf) - (p - buf), " %.256s", msg);
        message = buf;
    }
    if (msg[0] == '(') {
        PyErr_SetString(PyExc_SystemError, message);
    }
    else {
        PyErr_SetString(PyExc_TypeError, message);
    }
}


/* Convert a tuple argument.
   On entry, *p_format points to the character _after_ the opening '('.
   On successful exit, *p_format points to the closing ')'.
   If successful:
      *p_format and *p_va are updated,
      *levels and *msgbuf are untouched,
      and NULL is returned.
   If the argument is invalid:
      *p_format is unchanged,
      *p_va is undefined,
      *levels is a 0-terminated list of item numbers,
      *msgbuf contains an error message, whose format is:
     "must be <typename1>, not <typename2>", where:
        <typename1> is the name of the expected type, and
        <typename2> is the name of the actual type,
      and msgbuf is returned.
*/

static const char *
converttuple(PyObject *arg, const char **p_format, va_list *p_va, int flags,
             int *levels, char *msgbuf, size_t bufsize, int toplevel,
             freelist_t *freelist)
{
    int level = 0;
    int n = 0;
    const char *format = *p_format;
    int i;
    Py_ssize_t len;

    for (;;) {
        int c = *format++;
        if (c == '(') {
            if (level == 0)
                n++;
            level++;
        }
        else if (c == ')') {
            if (level == 0)
                break;
            level--;
        }
        else if (c == ':' || c == ';' || c == '\0')
            break;
        else if (level == 0 && Py_ISALPHA(Py_CHARMASK(c)))
            n++;
    }

    if (!PySequence_Check(arg) || PyBytes_Check(arg)) {
        levels[0] = 0;
        PyOS_snprintf(msgbuf, bufsize,
                      toplevel ? "expected %d arguments, not %.50s" :
                      "must be %d-item sequence, not %.50s",
                  n,
                  arg == Py_None ? "None" : arg->ob_type->tp_name);
        return msgbuf;
    }

    len = PySequence_Size(arg);
    if (len != n) {
        levels[0] = 0;
        if (toplevel) {
            PyOS_snprintf(msgbuf, bufsize,
                          "expected %d argument%s, not %" PY_FORMAT_SIZE_T "d",
                          n,
                          n == 1 ? "" : "s",
                          len);
        }
        else {
            PyOS_snprintf(msgbuf, bufsize,
                          "must be sequence of length %d, "
                          "not %" PY_FORMAT_SIZE_T "d",
                          n, len);
        }
        return msgbuf;
    }

    format = *p_format;
    for (i = 0; i < n; i++) {
        const char *msg;
        PyObject *item;
        item = PySequence_GetItem(arg, i);
        if (item == NULL) {
            PyErr_Clear();
            levels[0] = i+1;
            levels[1] = 0;
            strncpy(msgbuf, "is not retrievable", bufsize);
            return msgbuf;
        }
        msg = convertitem(item, &format, p_va, flags, levels+1,
                          msgbuf, bufsize, freelist);
        /* PySequence_GetItem calls tp->sq_item, which INCREFs */
        Py_XDECREF(item);
        if (msg != NULL) {
            levels[0] = i+1;
            return msg;
        }
    }

    *p_format = format;
    return NULL;
}


/* Convert a single item. */

static const char *
convertitem(PyObject *arg, const char **p_format, va_list *p_va, int flags,
            int *levels, char *msgbuf, size_t bufsize, freelist_t *freelist)
{
    const char *msg;
    const char *format = *p_format;

    if (*format == '(' /* ')' */) {
        format++;
        msg = converttuple(arg, &format, p_va, flags, levels, msgbuf,
                           bufsize, 0, freelist);
        if (msg == NULL)
            format++;
    }
    else {
        msg = convertsimple(arg, &format, p_va, flags,
                            msgbuf, bufsize, freelist);
        if (msg != NULL)
            levels[0] = 0;
    }
    if (msg == NULL)
        *p_format = format;
    return msg;
}



/* Format an error message generated by convertsimple(). */

static const char *
converterr(const char *expected, PyObject *arg, char *msgbuf, size_t bufsize)
{
    assert(expected != NULL);
    assert(arg != NULL);
    if (expected[0] == '(') {
        PyOS_snprintf(msgbuf, bufsize,
                      "%.100s", expected);
    }
    else {
        PyOS_snprintf(msgbuf, bufsize,
                      "must be %.50s, not %.50s", expected,
                      arg == Py_None ? "None" : arg->ob_type->tp_name);
    }
    return msgbuf;
}

#define CONV_UNICODE "(unicode conversion error)"

/* Explicitly check for float arguments when integers are expected.
   Return 1 for error, 0 if ok.
   XXX Should be removed after the end of the deprecation period in
   _PyLong_FromNbIndexOrNbInt. */
static int
float_argument_error(PyObject *arg)
{
    if (PyFloat_Check(arg)) {
        PyErr_SetString(PyExc_TypeError,
                        "integer argument expected, got float" );
        return 1;
    }
    else
        return 0;
}

/* Convert a non-tuple argument.  Return NULL if conversion went OK,
   or a string with a message describing the failure.  The message is
   formatted as "must be <desired type>, not <actual type>".
   When failing, an exception may or may not have been raised.
   Don't call if a tuple is expected.

   When you add new format codes, please don't forget poor skipitem() below.
*/

static const char *
convertsimple(PyObject *arg, const char **p_format, va_list *p_va, int flags,
              char *msgbuf, size_t bufsize, freelist_t *freelist)
{
    /* For # codes */
#define FETCH_SIZE      int *q=NULL;Py_ssize_t *q2=NULL;\
    if (flags & FLAG_SIZE_T) q2=va_arg(*p_va, Py_ssize_t*); \
    else { \
        if (PyErr_WarnEx(PyExc_DeprecationWarning, \
                    "PY_SSIZE_T_CLEAN will be required for '#' formats", 1)) { \
            return NULL; \
        } \
        q=va_arg(*p_va, int*); \
    }
#define STORE_SIZE(s)   \
    if (flags & FLAG_SIZE_T) \
        *q2=s; \
    else { \
        if (INT_MAX < s) { \
            PyErr_SetString(PyExc_OverflowError, \
                "size does not fit in an int"); \
            return converterr("", arg, msgbuf, bufsize); \
        } \
        *q = (int)s; \
    }
#define BUFFER_LEN      ((flags & FLAG_SIZE_T) ? *q2:*q)
#define RETURN_ERR_OCCURRED return msgbuf

    const char *format = *p_format;
    char c = *format++;
    const char *sarg;

    switch (c) {

    case 'b': { /* unsigned byte -- very short int */
        char *p = va_arg(*p_va, char *);
        long ival;
        if (float_argument_error(arg))
            RETURN_ERR_OCCURRED;
        ival = PyLong_AsLong(arg);
        if (ival == -1 && PyErr_Occurred())
            RETURN_ERR_OCCURRED;
        else if (ival < 0) {
            PyErr_SetString(PyExc_OverflowError,
                            "unsigned byte integer is less than minimum");
            RETURN_ERR_OCCURRED;
        }
        else if (ival > UCHAR_MAX) {
            PyErr_SetString(PyExc_OverflowError,
                            "unsigned byte integer is greater than maximum");
            RETURN_ERR_OCCURRED;
        }
        else
            *p = (unsigned char) ival;
        break;
    }

    case 'B': {/* byte sized bitfield - both signed and unsigned
                  values allowed */
        char *p = va_arg(*p_va, char *);
        long ival;
        if (float_argument_error(arg))
            RETURN_ERR_OCCURRED;
        ival = PyLong_AsUnsignedLongMask(arg);
        if (ival == -1 && PyErr_Occurred())
            RETURN_ERR_OCCURRED;
        else
            *p = (unsigned char) ival;
        break;
    }

    case 'h': {/* signed short int */
        short *p = va_arg(*p_va, short *);
        long ival;
        if (float_argument_error(arg))
            RETURN_ERR_OCCURRED;
        ival = PyLong_AsLong(arg);
        if (ival == -1 && PyErr_Occurred())
            RETURN_ERR_OCCURRED;
        else if (ival < SHRT_MIN) {
            PyErr_SetString(PyExc_OverflowError,
                            "signed short integer is less than minimum");
            RETURN_ERR_OCCURRED;
        }
        else if (ival > SHRT_MAX) {
            PyErr_SetString(PyExc_OverflowError,
                            "signed short integer is greater than maximum");
            RETURN_ERR_OCCURRED;
        }
        else
            *p = (short) ival;
        break;
    }

    case 'H': { /* short int sized bitfield, both signed and
                   unsigned allowed */
        unsigned short *p = va_arg(*p_va, unsigned short *);
        long ival;
        if (float_argument_error(arg))
            RETURN_ERR_OCCURRED;
        ival = PyLong_AsUnsignedLongMask(arg);
        if (ival == -1 && PyErr_Occurred())
            RETURN_ERR_OCCURRED;
        else
            *p = (unsigned short) ival;
        break;
    }

    case 'i': {/* signed int */
        int *p = va_arg(*p_va, int *);
        long ival;
        if (float_argument_error(arg))
            RETURN_ERR_OCCURRED;
        ival = PyLong_AsLong(arg);
        if (ival == -1 && PyErr_Occurred())
            RETURN_ERR_OCCURRED;
        else if (ival > INT_MAX) {
            PyErr_SetString(PyExc_OverflowError,
                            "signed integer is greater than maximum");
            RETURN_ERR_OCCURRED;
        }
        else if (ival < INT_MIN) {
            PyErr_SetString(PyExc_OverflowError,
                            "signed integer is less than minimum");
            RETURN_ERR_OCCURRED;
        }
        else
            *p = ival;
        break;
    }

    case 'I': { /* int sized bitfield, both signed and
                   unsigned allowed */
        unsigned int *p = va_arg(*p_va, unsigned int *);
        unsigned int ival;
        if (float_argument_error(arg))
            RETURN_ERR_OCCURRED;
        ival = (unsigned int)PyLong_AsUnsignedLongMask(arg);
        if (ival == (unsigned int)-1 && PyErr_Occurred())
            RETURN_ERR_OCCURRED;
        else
            *p = ival;
        break;
    }

    case 'n': /* Py_ssize_t */
    {
        PyObject *iobj;
        Py_ssize_t *p = va_arg(*p_va, Py_ssize_t *);
        Py_ssize_t ival = -1;
        if (float_argument_error(arg))
            RETURN_ERR_OCCURRED;
        iobj = PyNumber_Index(arg);
        if (iobj != NULL) {
            ival = PyLong_AsSsize_t(iobj);
            Py_DECREF(iobj);
        }
        if (ival == -1 && PyErr_Occurred())
            RETURN_ERR_OCCURRED;
        *p = ival;
        break;
    }
    case 'l': {/* long int */
        long *p = va_arg(*p_va, long *);
        long ival;
        if (float_argument_error(arg))
            RETURN_ERR_OCCURRED;
        ival = PyLong_AsLong(arg);
        if (ival == -1 && PyErr_Occurred())
            RETURN_ERR_OCCURRED;
        else
            *p = ival;
        break;
    }

    case 'k': { /* long sized bitfield */
        unsigned long *p = va_arg(*p_va, unsigned long *);
        unsigned long ival;
        if (PyLong_Check(arg))
            ival = PyLong_AsUnsignedLongMask(arg);
        else
            return converterr("int", arg, msgbuf, bufsize);
        *p = ival;
        break;
    }

    case 'L': {/* long long */
        long long *p = va_arg( *p_va, long long * );
        long long ival;
        if (float_argument_error(arg))
            RETURN_ERR_OCCURRED;
        ival = PyLong_AsLongLong(arg);
        if (ival == (long long)-1 && PyErr_Occurred())
            RETURN_ERR_OCCURRED;
        else
            *p = ival;
        break;
    }

    case 'K': { /* long long sized bitfield */
        unsigned long long *p = va_arg(*p_va, unsigned long long *);
        unsigned long long ival;
        if (PyLong_Check(arg))
            ival = PyLong_AsUnsignedLongLongMask(arg);
        else
            return converterr("int", arg, msgbuf, bufsize);
        *p = ival;
        break;
    }

    case 'f': {/* float */
        float *p = va_arg(*p_va, float *);
        double dval = PyFloat_AsDouble(arg);
        if (PyErr_Occurred())
            RETURN_ERR_OCCURRED;
        else
            *p = (float) dval;
        break;
    }

    case 'd': {/* double */
        double *p = va_arg(*p_va, double *);
        double dval = PyFloat_AsDouble(arg);
        if (PyErr_Occurred())
            RETURN_ERR_OCCURRED;
        else
            *p = dval;
        break;
    }

    case 'D': {/* complex double */
        Py_complex *p = va_arg(*p_va, Py_complex *);
        Py_complex cval;
        cval = PyComplex_AsCComplex(arg);
        if (PyErr_Occurred())
            RETURN_ERR_OCCURRED;
        else
            *p = cval;
        break;
    }

    case 'c': {/* char */
        char *p = va_arg(*p_va, char *);
        if (PyBytes_Check(arg) && PyBytes_Size(arg) == 1)
            *p = PyBytes_AS_STRING(arg)[0];
        else if (PyByteArray_Check(arg) && PyByteArray_Size(arg) == 1)
            *p = PyByteArray_AS_STRING(arg)[0];
        else
            return converterr("a byte string of length 1", arg, msgbuf, bufsize);
        break;
    }

    case 'C': {/* unicode char */
        int *p = va_arg(*p_va, int *);
        int kind;
        void *data;

        if (!PyUnicode_Check(arg))
            return converterr("a unicode character", arg, msgbuf, bufsize);

        if (PyUnicode_READY(arg))
            RETURN_ERR_OCCURRED;

        if (PyUnicode_GET_LENGTH(arg) != 1)
            return converterr("a unicode character", arg, msgbuf, bufsize);

        kind = PyUnicode_KIND(arg);
        data = PyUnicode_DATA(arg);
        *p = PyUnicode_READ(kind, data, 0);
        break;
    }

    case 'p': {/* boolean *p*redicate */
        int *p = va_arg(*p_va, int *);
        int val = PyObject_IsTrue(arg);
        if (val > 0)
            *p = 1;
        else if (val == 0)
            *p = 0;
        else
            RETURN_ERR_OCCURRED;
        break;
    }

    /* XXX WAAAAH!  's', 'y', 'z', 'u', 'Z', 'e', 'w' codes all
       need to be cleaned up! */

    case 'y': {/* any bytes-like object */
        void **p = (void **)va_arg(*p_va, char **);
        const char *buf;
        Py_ssize_t count;
        if (*format == '*') {
            if (getbuffer(arg, (Py_buffer*)p, &buf) < 0)
                return converterr(buf, arg, msgbuf, bufsize);
            format++;
            if (addcleanup(p, freelist, cleanup_buffer)) {
                return converterr(
                    "(cleanup problem)",
                    arg, msgbuf, bufsize);
            }
            break;
        }
        count = convertbuffer(arg, (const void **)p, &buf);
        if (count < 0)
            return converterr(buf, arg, msgbuf, bufsize);
        if (*format == '#') {
            FETCH_SIZE;
            STORE_SIZE(count);
            format++;
        } else {
            if (strlen(*p) != (size_t)count) {
                PyErr_SetString(PyExc_ValueError, "embedded null byte");
                RETURN_ERR_OCCURRED;
            }
        }
        break;
    }

    case 's': /* text string or bytes-like object */
    case 'z': /* text string, bytes-like object or None */
    {
        if (*format == '*') {
            /* "s*" or "z*" */
            Py_buffer *p = (Py_buffer *)va_arg(*p_va, Py_buffer *);

            if (c == 'z' && arg == Py_None)
                PyBuffer_FillInfo(p, NULL, NULL, 0, 1, 0);
            else if (PyUnicode_Check(arg)) {
                Py_ssize_t len;
                sarg = PyUnicode_AsUTF8AndSize(arg, &len);
                if (sarg == NULL)
                    return converterr(CONV_UNICODE,
                                      arg, msgbuf, bufsize);
                PyBuffer_FillInfo(p, arg, (void *)sarg, len, 1, 0);
            }
            else { /* any bytes-like object */
                const char *buf;
                if (getbuffer(arg, p, &buf) < 0)
                    return converterr(buf, arg, msgbuf, bufsize);
            }
            if (addcleanup(p, freelist, cleanup_buffer)) {
                return converterr(
                    "(cleanup problem)",
                    arg, msgbuf, bufsize);
            }
            format++;
        } else if (*format == '#') { /* a string or read-only bytes-like object */
            /* "s#" or "z#" */
            const void **p = (const void **)va_arg(*p_va, const char **);
            FETCH_SIZE;

            if (c == 'z' && arg == Py_None) {
                *p = NULL;
                STORE_SIZE(0);
            }
            else if (PyUnicode_Check(arg)) {
                Py_ssize_t len;
                sarg = PyUnicode_AsUTF8AndSize(arg, &len);
                if (sarg == NULL)
                    return converterr(CONV_UNICODE,
                                      arg, msgbuf, bufsize);
                *p = sarg;
                STORE_SIZE(len);
            }
            else { /* read-only bytes-like object */
                /* XXX Really? */
                const char *buf;
                Py_ssize_t count = convertbuffer(arg, p, &buf);
                if (count < 0)
                    return converterr(buf, arg, msgbuf, bufsize);
                STORE_SIZE(count);
            }
            format++;
        } else {
            /* "s" or "z" */
            const char **p = va_arg(*p_va, const char **);
            Py_ssize_t len;
            sarg = NULL;

            if (c == 'z' && arg == Py_None)
                *p = NULL;
            else if (PyUnicode_Check(arg)) {
                sarg = PyUnicode_AsUTF8AndSize(arg, &len);
                if (sarg == NULL)
                    return converterr(CONV_UNICODE,
                                      arg, msgbuf, bufsize);
                if (strlen(sarg) != (size_t)len) {
                    PyErr_SetString(PyExc_ValueError, "embedded null character");
                    RETURN_ERR_OCCURRED;
                }
                *p = sarg;
            }
            else
                return converterr(c == 'z' ? "str or None" : "str",
                                  arg, msgbuf, bufsize);
        }
        break;
    }

    case 'u': /* raw unicode buffer (Py_UNICODE *) */
    case 'Z': /* raw unicode buffer or None */
    {
        // TODO: Raise DeprecationWarning
_Py_COMP_DIAG_PUSH
_Py_COMP_DIAG_IGNORE_DEPR_DECLS
        Py_UNICODE **p = va_arg(*p_va, Py_UNICODE **);

        if (*format == '#') {
            /* "u#" or "Z#" */
            FETCH_SIZE;

            if (c == 'Z' && arg == Py_None) {
                *p = NULL;
                STORE_SIZE(0);
            }
            else if (PyUnicode_Check(arg)) {
                Py_ssize_t len;
                *p = PyUnicode_AsUnicodeAndSize(arg, &len);
                if (*p == NULL)
                    RETURN_ERR_OCCURRED;
                STORE_SIZE(len);
            }
            else
                return converterr(c == 'Z' ? "str or None" : "str",
                                  arg, msgbuf, bufsize);
            format++;
        } else {
            /* "u" or "Z" */
            if (c == 'Z' && arg == Py_None)
                *p = NULL;
            else if (PyUnicode_Check(arg)) {
                Py_ssize_t len;
                *p = PyUnicode_AsUnicodeAndSize(arg, &len);
                if (*p == NULL)
                    RETURN_ERR_OCCURRED;
                if (wcslen(*p) != (size_t)len) {
                    PyErr_SetString(PyExc_ValueError, "embedded null character");
                    RETURN_ERR_OCCURRED;
                }
            } else
                return converterr(c == 'Z' ? "str or None" : "str",
                                  arg, msgbuf, bufsize);
        }
        break;
_Py_COMP_DIAG_POP
    }

    case 'e': {/* encoded string */
        char **buffer;
        const char *encoding;
        PyObject *s;
        int recode_strings;
        Py_ssize_t size;
        const char *ptr;

        /* Get 'e' parameter: the encoding name */
        encoding = (const char *)va_arg(*p_va, const char *);
        if (encoding == NULL)
            encoding = PyUnicode_GetDefaultEncoding();

        /* Get output buffer parameter:
           's' (recode all objects via Unicode) or
           't' (only recode non-string objects)
        */
        if (*format == 's')
            recode_strings = 1;
        else if (*format == 't')
            recode_strings = 0;
        else
            return converterr(
                "(unknown parser marker combination)",
                arg, msgbuf, bufsize);
        buffer = (char **)va_arg(*p_va, char **);
        format++;
        if (buffer == NULL)
            return converterr("(buffer is NULL)",
                              arg, msgbuf, bufsize);

        /* Encode object */
        if (!recode_strings &&
            (PyBytes_Check(arg) || PyByteArray_Check(arg))) {
            s = arg;
            Py_INCREF(s);
            if (PyBytes_Check(arg)) {
                size = PyBytes_GET_SIZE(s);
                ptr = PyBytes_AS_STRING(s);
            }
            else {
                size = PyByteArray_GET_SIZE(s);
                ptr = PyByteArray_AS_STRING(s);
            }
        }
        else if (PyUnicode_Check(arg)) {
            /* Encode object; use default error handling */
            s = PyUnicode_AsEncodedString(arg,
                                          encoding,
                                          NULL);
            if (s == NULL)
                return converterr("(encoding failed)",
                                  arg, msgbuf, bufsize);
            assert(PyBytes_Check(s));
            size = PyBytes_GET_SIZE(s);
            ptr = PyBytes_AS_STRING(s);
            if (ptr == NULL)
                ptr = "";
        }
        else {
            return converterr(
                recode_strings ? "str" : "str, bytes or bytearray",
                arg, msgbuf, bufsize);
        }

        /* Write output; output is guaranteed to be 0-terminated */
        if (*format == '#') {
            /* Using buffer length parameter '#':

               - if *buffer is NULL, a new buffer of the
               needed size is allocated and the data
               copied into it; *buffer is updated to point
               to the new buffer; the caller is
               responsible for PyMem_Free()ing it after
               usage

               - if *buffer is not NULL, the data is
               copied to *buffer; *buffer_len has to be
               set to the size of the buffer on input;
               buffer overflow is signalled with an error;
               buffer has to provide enough room for the
               encoded string plus the trailing 0-byte

               - in both cases, *buffer_len is updated to
               the size of the buffer /excluding/ the
               trailing 0-byte

            */
            FETCH_SIZE;

            format++;
            if (q == NULL && q2 == NULL) {
                Py_DECREF(s);
                return converterr(
                    "(buffer_len is NULL)",
                    arg, msgbuf, bufsize);
            }
            if (*buffer == NULL) {
                *buffer = PyMem_NEW(char, size + 1);
                if (*buffer == NULL) {
                    Py_DECREF(s);
                    PyErr_NoMemory();
                    RETURN_ERR_OCCURRED;
                }
                if (addcleanup(*buffer, freelist, cleanup_ptr)) {
                    Py_DECREF(s);
                    return converterr(
                        "(cleanup problem)",
                        arg, msgbuf, bufsize);
                }
            } else {
                if (size + 1 > BUFFER_LEN) {
                    Py_DECREF(s);
                    PyErr_Format(PyExc_ValueError,
                                 "encoded string too long "
                                 "(%zd, maximum length %zd)",
                                 (Py_ssize_t)size, (Py_ssize_t)(BUFFER_LEN-1));
                    RETURN_ERR_OCCURRED;
                }
            }
            memcpy(*buffer, ptr, size+1);
            STORE_SIZE(size);
        } else {
            /* Using a 0-terminated buffer:

               - the encoded string has to be 0-terminated
               for this variant to work; if it is not, an
               error raised

               - a new buffer of the needed size is
               allocated and the data copied into it;
               *buffer is updated to point to the new
               buffer; the caller is responsible for
               PyMem_Free()ing it after usage

            */
            if ((Py_ssize_t)strlen(ptr) != size) {
                Py_DECREF(s);
                return converterr(
                    "encoded string without null bytes",
                    arg, msgbuf, bufsize);
            }
            *buffer = PyMem_NEW(char, size + 1);
            if (*buffer == NULL) {
                Py_DECREF(s);
                PyErr_NoMemory();
                RETURN_ERR_OCCURRED;
            }
            if (addcleanup(*buffer, freelist, cleanup_ptr)) {
                Py_DECREF(s);
                return converterr("(cleanup problem)",
                                arg, msgbuf, bufsize);
            }
            memcpy(*buffer, ptr, size+1);
        }
        Py_DECREF(s);
        break;
    }

    case 'S': { /* PyBytes object */
        PyObject **p = va_arg(*p_va, PyObject **);
        if (PyBytes_Check(arg))
            *p = arg;
        else
            return converterr("bytes", arg, msgbuf, bufsize);
        break;
    }

    case 'Y': { /* PyByteArray object */
        PyObject **p = va_arg(*p_va, PyObject **);
        if (PyByteArray_Check(arg))
            *p = arg;
        else
            return converterr("bytearray", arg, msgbuf, bufsize);
        break;
    }

    case 'U': { /* PyUnicode object */
        PyObject **p = va_arg(*p_va, PyObject **);
        if (PyUnicode_Check(arg)) {
            if (PyUnicode_READY(arg) == -1)
                RETURN_ERR_OCCURRED;
            *p = arg;
        }
        else
            return converterr("str", arg, msgbuf, bufsize);
        break;
    }

    case 'O': { /* object */
        PyTypeObject *type;
        PyObject **p;
        if (*format == '!') {
            type = va_arg(*p_va, PyTypeObject*);
            p = va_arg(*p_va, PyObject **);
            format++;
            if (PyType_IsSubtype(arg->ob_type, type))
                *p = arg;
            else
                return converterr(type->tp_name, arg, msgbuf, bufsize);

        }
        else if (*format == '&') {
            typedef int (*converter)(PyObject *, void *);
            converter convert = va_arg(*p_va, converter);
            void *addr = va_arg(*p_va, void *);
            int res;
            format++;
            if (! (res = (*convert)(arg, addr)))
                return converterr("(unspecified)",
                                  arg, msgbuf, bufsize);
            if (res == Py_CLEANUP_SUPPORTED &&
                addcleanup(addr, freelist, convert) == -1)
                return converterr("(cleanup problem)",
                                arg, msgbuf, bufsize);
        }
        else {
            p = va_arg(*p_va, PyObject **);
            *p = arg;
        }
        break;
    }


    case 'w': { /* "w*": memory buffer, read-write access */
        void **p = va_arg(*p_va, void **);

        if (*format != '*')
            return converterr(
                "(invalid use of 'w' format character)",
                arg, msgbuf, bufsize);
        format++;

        /* Caller is interested in Py_buffer, and the object
           supports it directly. */
        if (PyObject_GetBuffer(arg, (Py_buffer*)p, PyBUF_WRITABLE) < 0) {
            PyErr_Clear();
            return converterr("read-write bytes-like object",
                              arg, msgbuf, bufsize);
        }
        if (!PyBuffer_IsContiguous((Py_buffer*)p, 'C')) {
            PyBuffer_Release((Py_buffer*)p);
            return converterr("contiguous buffer", arg, msgbuf, bufsize);
        }
        if (addcleanup(p, freelist, cleanup_buffer)) {
            return converterr(
                "(cleanup problem)",
                arg, msgbuf, bufsize);
        }
        break;
    }

    default:
        return converterr("(impossible<bad format char>)", arg, msgbuf, bufsize);

    }

    *p_format = format;
    return NULL;

#undef FETCH_SIZE
#undef STORE_SIZE
#undef BUFFER_LEN
#undef RETURN_ERR_OCCURRED
}

static Py_ssize_t
convertbuffer(PyObject *arg, const void **p, const char **errmsg)
{
    PyBufferProcs *pb = Py_TYPE(arg)->tp_as_buffer;
    Py_ssize_t count;
    Py_buffer view;

    *errmsg = NULL;
    *p = NULL;
    if (pb != NULL && pb->bf_releasebuffer != NULL) {
        *errmsg = "read-only bytes-like object";
        return -1;
    }

    if (getbuffer(arg, &view, errmsg) < 0)
        return -1;
    count = view.len;
    *p = view.buf;
    PyBuffer_Release(&view);
    return count;
}

static int
getbuffer(PyObject *arg, Py_buffer *view, const char **errmsg)
{
    if (PyObject_GetBuffer(arg, view, PyBUF_SIMPLE) != 0) {
        *errmsg = "bytes-like object";
        return -1;
    }
    if (!PyBuffer_IsContiguous(view, 'C')) {
        PyBuffer_Release(view);
        *errmsg = "contiguous buffer";
        return -1;
    }
    return 0;
}

/* Support for keyword arguments donated by
   Geoff Philbrick <philbric@delphi.hks.com> */

/* Return false (0) for error, else true. */
int
CPyArg_ParseTupleAndKeywords(PyObject *args,
                             PyObject *keywords,
                             const char *format,
                             char **kwlist, ...)
{
    int retval;
    va_list va;

    if ((args == NULL || !PyTuple_Check(args)) ||
        (keywords != NULL && !PyDict_Check(keywords)) ||
        format == NULL ||
        kwlist == NULL)
    {
        PyErr_BadInternalCall();
        return 0;
    }

    va_start(va, kwlist);
    retval = vgetargskeywords(args, keywords, format, kwlist, &va, FLAG_SIZE_T);
    va_end(va);
    return retval;
}


int
CPyArg_VaParseTupleAndKeywords(PyObject *args,
                               PyObject *keywords,
                               const char *format,
                               char **kwlist, va_list va)
{
    int retval;
    va_list lva;

    if ((args == NULL || !PyTuple_Check(args)) ||
        (keywords != NULL && !PyDict_Check(keywords)) ||
        format == NULL ||
        kwlist == NULL)
    {
        PyErr_BadInternalCall();
        return 0;
    }

    va_copy(lva, va);

    retval = vgetargskeywords(args, keywords, format, kwlist, &lva, FLAG_SIZE_T);
    va_end(lva);
    return retval;
}

#define IS_END_OF_FORMAT(c) (c == '\0' || c == ';' || c == ':')

static int
vgetargskeywords(PyObject *args, PyObject *kwargs, const char *format,
                 char **kwlist, va_list *p_va, int flags)
{
    char msgbuf[512];
    int levels[32];
    const char *fname, *msg, *custom_msg;
    int min = INT_MAX;
    int max = INT_MAX;
    int required_kwonly_start = INT_MAX;
    int has_required_kws = 0;
    int i, pos, len;
    int skip = 0;
    Py_ssize_t nargs, nkwargs;
    PyObject *current_arg;
    freelistentry_t static_entries[STATIC_FREELIST_ENTRIES];
    freelist_t freelist;
    int bound_pos_args;

    PyObject **p_args = NULL, **p_kwargs = NULL;

    freelist.entries = static_entries;
    freelist.first_available = 0;
    freelist.entries_malloced = 0;

    assert(args != NULL && PyTuple_Check(args));
    assert(kwargs == NULL || PyDict_Check(kwargs));
    assert(format != NULL);
    assert(kwlist != NULL);
    assert(p_va != NULL);

    /* grab the function name or custom error msg first (mutually exclusive) */
    fname = strchr(format, ':');
    if (fname) {
        fname++;
        custom_msg = NULL;
    }
    else {
        custom_msg = strchr(format,';');
        if (custom_msg)
            custom_msg++;
    }

    /* scan kwlist and count the number of positional-only parameters */
    for (pos = 0; kwlist[pos] && !*kwlist[pos]; pos++) {
    }
    /* scan kwlist and get greatest possible nbr of args */
    for (len = pos; kwlist[len]; len++) {
        if (!*kwlist[len]) {
            PyErr_SetString(PyExc_SystemError,
                            "Empty keyword parameter name");
            return cleanreturn(0, &freelist);
        }
    }

    if (*format == '%') {
        p_args = va_arg(*p_va, PyObject **);
        p_kwargs = va_arg(*p_va, PyObject **);
        format++;
    }

    if (len > STATIC_FREELIST_ENTRIES) {
        freelist.entries = PyMem_NEW(freelistentry_t, len);
        if (freelist.entries == NULL) {
            PyErr_NoMemory();
            return 0;
        }
        freelist.entries_malloced = 1;
    }

    nargs = PyTuple_GET_SIZE(args);
    nkwargs = (kwargs == NULL) ? 0 : PyDict_GET_SIZE(kwargs);
    if (nargs + nkwargs > len && !p_args && !p_kwargs) {
        /* Adding "keyword" (when nargs == 0) prevents producing wrong error
           messages in some special cases (see bpo-31229). */
        PyErr_Format(PyExc_TypeError,
                     "%.200s%s takes at most %d %sargument%s (%zd given)",
                     (fname == NULL) ? "function" : fname,
                     (fname == NULL) ? "" : "()",
                     len,
                     (nargs == 0) ? "keyword " : "",
                     (len == 1) ? "" : "s",
                     nargs + nkwargs);
        return cleanreturn(0, &freelist);
    }

    /* convert tuple args and keyword args in same loop, using kwlist to drive process */
    for (i = 0; i < len; i++) {
        if (*format == '|') {
            if (min != INT_MAX) {
                PyErr_SetString(PyExc_SystemError,
                                "Invalid format string (| specified twice)");
                return cleanreturn(0, &freelist);
            }

            min = i;
            format++;

            if (max != INT_MAX) {
                PyErr_SetString(PyExc_SystemError,
                                "Invalid format string ($ before |)");
                return cleanreturn(0, &freelist);
            }

            /* If there are optional args, figure out whether we have
             * required keyword arguments so that we don't bail without
             * enforcing them. */
            has_required_kws = strchr(format, '@') != NULL;
        }
        if (*format == '$') {
            if (max != INT_MAX) {
                PyErr_SetString(PyExc_SystemError,
                                "Invalid format string ($ specified twice)");
                return cleanreturn(0, &freelist);
            }

            max = i;
            format++;

            if (max < pos) {
                PyErr_SetString(PyExc_SystemError,
                                "Empty parameter name after $");
                return cleanreturn(0, &freelist);
            }
            if (skip) {
                /* Now we know the minimal and the maximal numbers of
                 * positional arguments and can raise an exception with
                 * informative message (see below). */
                break;
            }
            if (max < nargs && !p_args) {
                if (max == 0) {
                    PyErr_Format(PyExc_TypeError,
                                 "%.200s%s takes no positional arguments",
                                 (fname == NULL) ? "function" : fname,
                                 (fname == NULL) ? "" : "()");
                }
                else {
                    PyErr_Format(PyExc_TypeError,
                                 "%.200s%s takes %s %d positional argument%s"
                                 " (%zd given)",
                                 (fname == NULL) ? "function" : fname,
                                 (fname == NULL) ? "" : "()",
                                 (min < max) ? "at most" : "exactly",
                                 max,
                                 max == 1 ? "" : "s",
                                 nargs);
                }
                return cleanreturn(0, &freelist);
            }
        }
        if (*format == '@') {
            if (min == INT_MAX && max == INT_MAX) {
                PyErr_SetString(PyExc_SystemError,
                                "Invalid format string "
                                "(@ without preceding | and $)");
                return cleanreturn(0, &freelist);
            }
            if (required_kwonly_start != INT_MAX) {
                PyErr_SetString(PyExc_SystemError,
                                "Invalid format string (@ specified twice)");
                return cleanreturn(0, &freelist);
            }

            required_kwonly_start = i;
            format++;
        }
        if (IS_END_OF_FORMAT(*format)) {
            PyErr_Format(PyExc_SystemError,
                         "More keyword list entries (%d) than "
                         "format specifiers (%d)", len, i);
            return cleanreturn(0, &freelist);
        }
        if (!skip) {
            if (i < nargs && i < max) {
                current_arg = PyTuple_GET_ITEM(args, i);
            }
            else if (nkwargs && i >= pos) {
                current_arg = _PyDict_GetItemStringWithError(kwargs, kwlist[i]);
                if (current_arg) {
                    --nkwargs;
                }
                else if (PyErr_Occurred()) {
                    return cleanreturn(0, &freelist);
                }
            }
            else {
                current_arg = NULL;
            }

            if (current_arg) {
                msg = convertitem(current_arg, &format, p_va, flags,
                    levels, msgbuf, sizeof(msgbuf), &freelist);
                if (msg) {
                    seterror(i+1, msg, levels, fname, custom_msg);
                    return cleanreturn(0, &freelist);
                }
                continue;
            }

            if (i < min || i >= required_kwonly_start) {
                if (i < pos) {
                    assert (min == INT_MAX);
                    assert (max == INT_MAX);
                    skip = 1;
                    /* At that moment we still don't know the minimal and
                     * the maximal numbers of positional arguments.  Raising
                     * an exception is deferred until we encounter | and $
                     * or the end of the format. */
                }
                else {
                    if (i >= max) {
                        PyErr_Format(PyExc_TypeError,
                                     "%.200s%s missing required "
                                     "keyword-only argument '%s'",
                                     (fname == NULL) ? "function" : fname,
                                     (fname == NULL) ? "" : "()",
                                     kwlist[i]);
                    }
                    else {
                        PyErr_Format(PyExc_TypeError,
                                     "%.200s%s missing required "
                                     "argument '%s' (pos %d)",
                                     (fname == NULL) ? "function" : fname,
                                     (fname == NULL) ? "" : "()",
                                     kwlist[i], i+1);
                    }
                    return cleanreturn(0, &freelist);
                }
            }
            /* current code reports success when all required args
             * fulfilled and no keyword args left, with no further
             * validation. XXX Maybe skip this in debug build ?
             */
            if (!nkwargs && !skip && !has_required_kws &&
                !p_args && !p_kwargs)
            {
                return cleanreturn(1, &freelist);
            }
        }

        /* We are into optional args, skip through to any remaining
         * keyword args */
        msg = skipitem(&format, p_va, flags);
        if (msg) {
            PyErr_Format(PyExc_SystemError, "%s: '%s'", msg,
                         format);
            return cleanreturn(0, &freelist);
        }
    }

    if (skip) {
        PyErr_Format(PyExc_TypeError,
                     "%.200s%s takes %s %d positional argument%s"
                     " (%zd given)",
                     (fname == NULL) ? "function" : fname,
                     (fname == NULL) ? "" : "()",
                     (Py_MIN(pos, min) < i) ? "at least" : "exactly",
                     Py_MIN(pos, min),
                     Py_MIN(pos, min) == 1 ? "" : "s",
                     nargs);
        return cleanreturn(0, &freelist);
    }

    if (!IS_END_OF_FORMAT(*format) &&
        (*format != '|') && (*format != '$') && (*format != '@'))
    {
        PyErr_Format(PyExc_SystemError,
            "more argument specifiers than keyword list entries "
            "(remaining format:'%s')", format);
        return cleanreturn(0, &freelist);
    }

    bound_pos_args = Py_MIN(nargs, Py_MIN(max, len));
    if (p_args) {
        *p_args = PyTuple_GetSlice(args, bound_pos_args, nargs);
        if (!*p_args) {
            return cleanreturn(0, &freelist);
        }
    }

    if (p_kwargs) {
        /* This unfortunately needs to be special cased because if len is 0 then we
         * never go through the main loop. */
        if (nargs > 0 && len == 0 && !p_args) {
            PyErr_Format(PyExc_TypeError,
                         "%.200s%s takes no positional arguments",
                         (fname == NULL) ? "function" : fname,
                         (fname == NULL) ? "" : "()");

            return cleanreturn(0, &freelist);
        }

        *p_kwargs = PyDict_New();
        if (!*p_kwargs) {
            goto latefail;
        }
    }

    if (nkwargs > 0) {
        PyObject *key, *value;
        Py_ssize_t j;
        /* make sure there are no arguments given by name and position */
        for (i = pos; i < bound_pos_args && i < len; i++) {
            current_arg = _PyDict_GetItemStringWithError(kwargs, kwlist[i]);
            if (current_arg) {
                /* arg present in tuple and in dict */
                PyErr_Format(PyExc_TypeError,
                             "argument for %.200s%s given by name ('%s') "
                             "and position (%d)",
                             (fname == NULL) ? "function" : fname,
                             (fname == NULL) ? "" : "()",
                             kwlist[i], i+1);
                goto latefail;
            }
            else if (PyErr_Occurred()) {
                goto latefail;
            }
        }
        /* make sure there are no extraneous keyword arguments */
        j = 0;
        while (PyDict_Next(kwargs, &j, &key, &value)) {
            int match = 0;
            if (!PyUnicode_Check(key)) {
                PyErr_SetString(PyExc_TypeError,
                                "keywords must be strings");
                goto latefail;
            }
            for (i = pos; i < len; i++) {
                if (CPyUnicode_EqualToASCIIString(key, kwlist[i])) {
                    match = 1;
                    break;
                }
            }
            if (!match) {
                if (!p_kwargs) {
                    PyErr_Format(PyExc_TypeError,
                                 "'%U' is an invalid keyword "
                                 "argument for %.200s%s",
                                 key,
                                 (fname == NULL) ? "this function" : fname,
                                 (fname == NULL) ? "" : "()");
                    goto latefail;
                } else {
                    if (PyDict_SetItem(*p_kwargs, key, value) < 0) {
                        goto latefail;
                    }
                }
            }
        }
    }

    return cleanreturn(1, &freelist);
    /* Handle failures that have happened after we have tried to
     * create *args and **kwargs, if they exist. */
latefail:
    if (p_args) {
        Py_XDECREF(*p_args);
    }
    if (p_kwargs) {
        Py_XDECREF(*p_kwargs);
    }
    return cleanreturn(0, &freelist);
}


static const char *
skipitem(const char **p_format, va_list *p_va, int flags)
{
    const char *format = *p_format;
    char c = *format++;

    switch (c) {

    /*
     * codes that take a single data pointer as an argument
     * (the type of the pointer is irrelevant)
     */

    case 'b': /* byte -- very short int */
    case 'B': /* byte as bitfield */
    case 'h': /* short int */
    case 'H': /* short int as bitfield */
    case 'i': /* int */
    case 'I': /* int sized bitfield */
    case 'l': /* long int */
    case 'k': /* long int sized bitfield */
    case 'L': /* long long */
    case 'K': /* long long sized bitfield */
    case 'n': /* Py_ssize_t */
    case 'f': /* float */
    case 'd': /* double */
    case 'D': /* complex double */
    case 'c': /* char */
    case 'C': /* unicode char */
    case 'p': /* boolean predicate */
    case 'S': /* string object */
    case 'Y': /* string object */
    case 'U': /* unicode string object */
        {
            if (p_va != NULL) {
                (void) va_arg(*p_va, void *);
            }
            break;
        }

    /* string codes */

    case 'e': /* string with encoding */
        {
            if (p_va != NULL) {
                (void) va_arg(*p_va, const char *);
            }
            if (!(*format == 's' || *format == 't'))
                /* after 'e', only 's' and 't' is allowed */
                goto err;
            format++;
        }
        /* fall through */

    case 's': /* string */
    case 'z': /* string or None */
    case 'y': /* bytes */
    case 'u': /* unicode string */
    case 'Z': /* unicode string or None */
    case 'w': /* buffer, read-write */
        {
            if (p_va != NULL) {
                (void) va_arg(*p_va, char **);
            }
            if (*format == '#') {
                if (p_va != NULL) {
                    if (flags & FLAG_SIZE_T)
                        (void) va_arg(*p_va, Py_ssize_t *);
                    else {
                        if (PyErr_WarnEx(PyExc_DeprecationWarning,
                                    "PY_SSIZE_T_CLEAN will be required for '#' formats", 1)) {
                            return NULL;
                        }
                        (void) va_arg(*p_va, int *);
                    }
                }
                format++;
            } else if ((c == 's' || c == 'z' || c == 'y' || c == 'w')
                       && *format == '*')
            {
                format++;
            }
            break;
        }

    case 'O': /* object */
        {
            if (*format == '!') {
                format++;
                if (p_va != NULL) {
                    (void) va_arg(*p_va, PyTypeObject*);
                    (void) va_arg(*p_va, PyObject **);
                }
            }
            else if (*format == '&') {
                typedef int (*converter)(PyObject *, void *);
                if (p_va != NULL) {
                    (void) va_arg(*p_va, converter);
                    (void) va_arg(*p_va, void *);
                }
                format++;
            }
            else {
                if (p_va != NULL) {
                    (void) va_arg(*p_va, PyObject **);
                }
            }
            break;
        }

    case '(':           /* bypass tuple, not handled at all previously */
        {
            const char *msg;
            for (;;) {
                if (*format==')')
                    break;
                if (IS_END_OF_FORMAT(*format))
                    return "Unmatched left paren in format "
                           "string";
                msg = skipitem(&format, p_va, flags);
                if (msg)
                    return msg;
            }
            format++;
            break;
        }

    case ')':
        return "Unmatched right paren in format string";

    default:
err:
        return "impossible<bad format char>";

    }

    *p_format = format;
    return NULL;
}

#ifdef __cplusplus
};
#endif
