#ifndef Py_COMPILE_H
#define Py_COMPILE_H

#ifndef Py_LIMITED_API
#include "code.h"

#ifdef __cplusplus
extern "C" {
#endif

/* Public interface */
struct _node; /* Declare the existence of this type */
PyAPI_FUNC(PyCodeObject *) PyNode_Compile(struct _node *, const char *);
/* XXX (ncoghlan): Unprefixed type name in a public API! */

#define PyCF_MASK (CO_FUTURE_DIVISION | CO_FUTURE_ABSOLUTE_IMPORT | \
                   CO_FUTURE_WITH_STATEMENT | CO_FUTURE_PRINT_FUNCTION | \
                   CO_FUTURE_UNICODE_LITERALS | CO_FUTURE_BARRY_AS_BDFL | \
                   CO_FUTURE_GENERATOR_STOP | CO_FUTURE_ANNOTATIONS)
#define PyCF_MASK_OBSOLETE (CO_NESTED)
#define PyCF_SOURCE_IS_UTF8  0x0100
#define PyCF_DONT_IMPLY_DEDENT 0x0200
#define PyCF_ONLY_AST 0x0400
#define PyCF_IGNORE_COOKIE 0x0800

#ifndef Py_LIMITED_API
typedef struct {
    int cf_flags;  /* bitmask of CO_xxx flags relevant to future */
} PyCompilerFlags;
#endif

/* Future feature support */

typedef struct {
    int ff_features;      /* flags set by future statements */
    int ff_lineno;        /* line number of last future statement */
} PyFutureFeatures;

#define FUTURE_NESTED_SCOPES "nested_scopes"
#define FUTURE_GENERATORS "generators"
#define FUTURE_DIVISION "division"
#define FUTURE_ABSOLUTE_IMPORT "absolute_import"
#define FUTURE_WITH_STATEMENT "with_statement"
#define FUTURE_PRINT_FUNCTION "print_function"
#define FUTURE_UNICODE_LITERALS "unicode_literals"
#define FUTURE_BARRY_AS_BDFL "barry_as_FLUFL"
#define FUTURE_GENERATOR_STOP "generator_stop"
#define FUTURE_ANNOTATIONS "annotations"

struct _mod; /* Declare the existence of this type */
#define PyAST_Compile(mod, s, f, ar) PyAST_CompileEx(mod, s, f, -1, ar)
PyAPI_FUNC(PyCodeObject *) PyAST_CompileEx(
    struct _mod *mod,
    const char *filename,       /* decoded from the filesystem encoding */
    PyCompilerFlags *flags,
    int optimize,
    PyArena *arena);
PyAPI_FUNC(PyCodeObject *) PyAST_CompileObject(
    struct _mod *mod,
    PyObject *filename,
    PyCompilerFlags *flags,
    int optimize,
    PyArena *arena);
PyAPI_FUNC(PyFutureFeatures *) PyFuture_FromAST(
    struct _mod * mod,
    const char *filename        /* decoded from the filesystem encoding */
    );
PyAPI_FUNC(PyFutureFeatures *) PyFuture_FromASTObject(
    struct _mod * mod,
    PyObject *filename
    );

/* _Py_Mangle is defined in compile.c */
PyAPI_FUNC(PyObject*) _Py_Mangle(PyObject *p, PyObject *name);

#define PY_INVALID_STACK_EFFECT INT_MAX
PyAPI_FUNC(int) PyCompile_OpcodeStackEffect(int opcode, int oparg);

PyAPI_FUNC(int) _PyAST_Optimize(struct _mod *, PyArena *arena, int optimize);

#ifdef __cplusplus
}
#endif

#endif /* !Py_LIMITED_API */

/* These definitions must match corresponding definitions in graminit.h.
   There's code in compile.c that checks that they are the same. */
#define Py_single_input 256
#define Py_file_input 257
#define Py_eval_input 258

#endif /* !Py_COMPILE_H */
