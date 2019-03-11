
#ifndef Py_MODSUPPORT_H
#define Py_MODSUPPORT_H
#ifdef __cplusplus
extern "C" {
#endif

/* Module support interface */

#include <stdarg.h>

/* If PY_SSIZE_T_CLEAN is defined, each functions treats #-specifier
   to mean Py_ssize_t */
#ifdef PY_SSIZE_T_CLEAN
#define PyArg_Parse                     _PyArg_Parse_SizeT
#define PyArg_ParseTuple                _PyArg_ParseTuple_SizeT
#define PyArg_ParseTupleAndKeywords     _PyArg_ParseTupleAndKeywords_SizeT
#define PyArg_VaParse                   _PyArg_VaParse_SizeT
#define PyArg_VaParseTupleAndKeywords   _PyArg_VaParseTupleAndKeywords_SizeT
#define Py_BuildValue                   _Py_BuildValue_SizeT
#define Py_VaBuildValue                 _Py_VaBuildValue_SizeT
#ifndef Py_LIMITED_API
#define _Py_VaBuildStack                _Py_VaBuildStack_SizeT
#endif
#else
#ifndef Py_LIMITED_API
PyAPI_FUNC(PyObject *) _Py_VaBuildValue_SizeT(const char *, va_list);
PyAPI_FUNC(PyObject **) _Py_VaBuildStack_SizeT(
    PyObject **small_stack,
    Py_ssize_t small_stack_len,
    const char *format,
    va_list va,
    Py_ssize_t *p_nargs);
#endif /* !Py_LIMITED_API */
#endif

/* Due to a glitch in 3.2, the _SizeT versions weren't exported from the DLL. */
#if !defined(PY_SSIZE_T_CLEAN) || !defined(Py_LIMITED_API) || Py_LIMITED_API+0 >= 0x03030000
PyAPI_FUNC(int) PyArg_Parse(PyObject *, const char *, ...);
PyAPI_FUNC(int) PyArg_ParseTuple(PyObject *, const char *, ...);
PyAPI_FUNC(int) PyArg_ParseTupleAndKeywords(PyObject *, PyObject *,
                                                  const char *, char **, ...);
PyAPI_FUNC(int) PyArg_VaParse(PyObject *, const char *, va_list);
PyAPI_FUNC(int) PyArg_VaParseTupleAndKeywords(PyObject *, PyObject *,
                                                  const char *, char **, va_list);
#endif
PyAPI_FUNC(int) PyArg_ValidateKeywordArguments(PyObject *);
PyAPI_FUNC(int) PyArg_UnpackTuple(PyObject *, const char *, Py_ssize_t, Py_ssize_t, ...);
PyAPI_FUNC(PyObject *) Py_BuildValue(const char *, ...);
PyAPI_FUNC(PyObject *) _Py_BuildValue_SizeT(const char *, ...);


#ifndef Py_LIMITED_API
PyAPI_FUNC(int) _PyArg_UnpackStack(
    PyObject *const *args,
    Py_ssize_t nargs,
    const char *name,
    Py_ssize_t min,
    Py_ssize_t max,
    ...);

PyAPI_FUNC(int) _PyArg_NoKeywords(const char *funcname, PyObject *kwargs);
PyAPI_FUNC(int) _PyArg_NoPositional(const char *funcname, PyObject *args);
#define _PyArg_NoKeywords(funcname, kwargs) \
    ((kwargs) == NULL || _PyArg_NoKeywords((funcname), (kwargs)))
#define _PyArg_NoPositional(funcname, args) \
    ((args) == NULL || _PyArg_NoPositional((funcname), (args)))

#endif

PyAPI_FUNC(PyObject *) Py_VaBuildValue(const char *, va_list);
#ifndef Py_LIMITED_API
PyAPI_FUNC(PyObject **) _Py_VaBuildStack(
    PyObject **small_stack,
    Py_ssize_t small_stack_len,
    const char *format,
    va_list va,
    Py_ssize_t *p_nargs);
#endif

#ifndef Py_LIMITED_API
typedef struct _PyArg_Parser {
    const char *format;
    const char * const *keywords;
    const char *fname;
    const char *custom_msg;
    int pos;            /* number of positional-only arguments */
    int min;            /* minimal number of arguments */
    int max;            /* maximal number of positional arguments */
    PyObject *kwtuple;  /* tuple of keyword parameter names */
    struct _PyArg_Parser *next;
} _PyArg_Parser;
#ifdef PY_SSIZE_T_CLEAN
#define _PyArg_ParseTupleAndKeywordsFast  _PyArg_ParseTupleAndKeywordsFast_SizeT
#define _PyArg_ParseStack  _PyArg_ParseStack_SizeT
#define _PyArg_ParseStackAndKeywords  _PyArg_ParseStackAndKeywords_SizeT
#define _PyArg_VaParseTupleAndKeywordsFast  _PyArg_VaParseTupleAndKeywordsFast_SizeT
#endif
PyAPI_FUNC(int) _PyArg_ParseTupleAndKeywordsFast(PyObject *, PyObject *,
                                                 struct _PyArg_Parser *, ...);
PyAPI_FUNC(int) _PyArg_ParseStack(
    PyObject *const *args,
    Py_ssize_t nargs,
    const char *format,
    ...);
PyAPI_FUNC(int) _PyArg_ParseStackAndKeywords(
    PyObject *const *args,
    Py_ssize_t nargs,
    PyObject *kwnames,
    struct _PyArg_Parser *,
    ...);
PyAPI_FUNC(int) _PyArg_VaParseTupleAndKeywordsFast(PyObject *, PyObject *,
                                                   struct _PyArg_Parser *, va_list);
void _PyArg_Fini(void);
#endif   /* Py_LIMITED_API */

PyAPI_FUNC(int) PyModule_AddObject(PyObject *, const char *, PyObject *);
PyAPI_FUNC(int) PyModule_AddIntConstant(PyObject *, const char *, long);
PyAPI_FUNC(int) PyModule_AddStringConstant(PyObject *, const char *, const char *);
#define PyModule_AddIntMacro(m, c) PyModule_AddIntConstant(m, #c, c)
#define PyModule_AddStringMacro(m, c) PyModule_AddStringConstant(m, #c, c)

#if !defined(Py_LIMITED_API) || Py_LIMITED_API+0 >= 0x03050000
/* New in 3.5 */
PyAPI_FUNC(int) PyModule_SetDocString(PyObject *, const char *);
PyAPI_FUNC(int) PyModule_AddFunctions(PyObject *, PyMethodDef *);
PyAPI_FUNC(int) PyModule_ExecDef(PyObject *module, PyModuleDef *def);
#endif

#define Py_CLEANUP_SUPPORTED 0x20000

#define PYTHON_API_VERSION 1013
#define PYTHON_API_STRING "1013"
/* The API version is maintained (independently from the Python version)
   so we can detect mismatches between the interpreter and dynamically
   loaded modules.  These are diagnosed by an error message but
   the module is still loaded (because the mismatch can only be tested
   after loading the module).  The error message is intended to
   explain the core dump a few seconds later.

   The symbol PYTHON_API_STRING defines the same value as a string
   literal.  *** PLEASE MAKE SURE THE DEFINITIONS MATCH. ***

   Please add a line or two to the top of this log for each API
   version change:

   22-Feb-2006  MvL     1013    PEP 353 - long indices for sequence lengths

   19-Aug-2002  GvR     1012    Changes to string object struct for
                                interning changes, saving 3 bytes.

   17-Jul-2001  GvR     1011    Descr-branch, just to be on the safe side

   25-Jan-2001  FLD     1010    Parameters added to PyCode_New() and
                                PyFrame_New(); Python 2.1a2

   14-Mar-2000  GvR     1009    Unicode API added

   3-Jan-1999   GvR     1007    Decided to change back!  (Don't reuse 1008!)

   3-Dec-1998   GvR     1008    Python 1.5.2b1

   18-Jan-1997  GvR     1007    string interning and other speedups

   11-Oct-1996  GvR     renamed Py_Ellipses to Py_Ellipsis :-(

   30-Jul-1996  GvR     Slice and ellipses syntax added

   23-Jul-1996  GvR     For 1.4 -- better safe than sorry this time :-)

   7-Nov-1995   GvR     Keyword arguments (should've been done at 1.3 :-( )

   10-Jan-1995  GvR     Renamed globals to new naming scheme

   9-Jan-1995   GvR     Initial version (incompatible with older API)
*/

/* The PYTHON_ABI_VERSION is introduced in PEP 384. For the lifetime of
   Python 3, it will stay at the value of 3; changes to the limited API
   must be performed in a strictly backwards-compatible manner. */
#define PYTHON_ABI_VERSION 3
#define PYTHON_ABI_STRING "3"

#ifdef Py_TRACE_REFS
 /* When we are tracing reference counts, rename module creation functions so
    modules compiled with incompatible settings will generate a
    link-time error. */
 #define PyModule_Create2 PyModule_Create2TraceRefs
 #define PyModule_FromDefAndSpec2 PyModule_FromDefAndSpec2TraceRefs
#endif

PyAPI_FUNC(PyObject *) PyModule_Create2(struct PyModuleDef*,
                                     int apiver);
#ifndef Py_LIMITED_API
PyAPI_FUNC(PyObject *) _PyModule_CreateInitialized(struct PyModuleDef*,
                                                   int apiver);
#endif

#ifdef Py_LIMITED_API
#define PyModule_Create(module) \
        PyModule_Create2(module, PYTHON_ABI_VERSION)
#else
#define PyModule_Create(module) \
        PyModule_Create2(module, PYTHON_API_VERSION)
#endif

#if !defined(Py_LIMITED_API) || Py_LIMITED_API+0 >= 0x03050000
/* New in 3.5 */
PyAPI_FUNC(PyObject *) PyModule_FromDefAndSpec2(PyModuleDef *def,
                                                PyObject *spec,
                                                int module_api_version);

#ifdef Py_LIMITED_API
#define PyModule_FromDefAndSpec(module, spec) \
    PyModule_FromDefAndSpec2(module, spec, PYTHON_ABI_VERSION)
#else
#define PyModule_FromDefAndSpec(module, spec) \
    PyModule_FromDefAndSpec2(module, spec, PYTHON_API_VERSION)
#endif /* Py_LIMITED_API */
#endif /* New in 3.5 */

#ifndef Py_LIMITED_API
PyAPI_DATA(const char *) _Py_PackageContext;
#endif

#ifdef __cplusplus
}
#endif
#endif /* !Py_MODSUPPORT_H */
