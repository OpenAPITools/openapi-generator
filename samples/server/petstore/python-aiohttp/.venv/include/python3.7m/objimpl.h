/* The PyObject_ memory family:  high-level object memory interfaces.
   See pymem.h for the low-level PyMem_ family.
*/

#ifndef Py_OBJIMPL_H
#define Py_OBJIMPL_H

#include "pymem.h"

#ifdef __cplusplus
extern "C" {
#endif

/* BEWARE:

   Each interface exports both functions and macros.  Extension modules should
   use the functions, to ensure binary compatibility across Python versions.
   Because the Python implementation is free to change internal details, and
   the macros may (or may not) expose details for speed, if you do use the
   macros you must recompile your extensions with each Python release.

   Never mix calls to PyObject_ memory functions with calls to the platform
   malloc/realloc/ calloc/free, or with calls to PyMem_.
*/

/*
Functions and macros for modules that implement new object types.

 - PyObject_New(type, typeobj) allocates memory for a new object of the given
   type, and initializes part of it.  'type' must be the C structure type used
   to represent the object, and 'typeobj' the address of the corresponding
   type object.  Reference count and type pointer are filled in; the rest of
   the bytes of the object are *undefined*!  The resulting expression type is
   'type *'.  The size of the object is determined by the tp_basicsize field
   of the type object.

 - PyObject_NewVar(type, typeobj, n) is similar but allocates a variable-size
   object with room for n items.  In addition to the refcount and type pointer
   fields, this also fills in the ob_size field.

 - PyObject_Del(op) releases the memory allocated for an object.  It does not
   run a destructor -- it only frees the memory.  PyObject_Free is identical.

 - PyObject_Init(op, typeobj) and PyObject_InitVar(op, typeobj, n) don't
   allocate memory.  Instead of a 'type' parameter, they take a pointer to a
   new object (allocated by an arbitrary allocator), and initialize its object
   header fields.

Note that objects created with PyObject_{New, NewVar} are allocated using the
specialized Python allocator (implemented in obmalloc.c), if WITH_PYMALLOC is
enabled.  In addition, a special debugging allocator is used if PYMALLOC_DEBUG
is also #defined.

In case a specific form of memory management is needed (for example, if you
must use the platform malloc heap(s), or shared memory, or C++ local storage or
operator new), you must first allocate the object with your custom allocator,
then pass its pointer to PyObject_{Init, InitVar} for filling in its Python-
specific fields:  reference count, type pointer, possibly others.  You should
be aware that Python has no control over these objects because they don't
cooperate with the Python memory manager.  Such objects may not be eligible
for automatic garbage collection and you have to make sure that they are
released accordingly whenever their destructor gets called (cf. the specific
form of memory management you're using).

Unless you have specific memory management requirements, use
PyObject_{New, NewVar, Del}.
*/

/*
 * Raw object memory interface
 * ===========================
 */

/* Functions to call the same malloc/realloc/free as used by Python's
   object allocator.  If WITH_PYMALLOC is enabled, these may differ from
   the platform malloc/realloc/free.  The Python object allocator is
   designed for fast, cache-conscious allocation of many "small" objects,
   and with low hidden memory overhead.

   PyObject_Malloc(0) returns a unique non-NULL pointer if possible.

   PyObject_Realloc(NULL, n) acts like PyObject_Malloc(n).
   PyObject_Realloc(p != NULL, 0) does not return  NULL, or free the memory
   at p.

   Returned pointers must be checked for NULL explicitly; no action is
   performed on failure other than to return NULL (no warning it printed, no
   exception is set, etc).

   For allocating objects, use PyObject_{New, NewVar} instead whenever
   possible.  The PyObject_{Malloc, Realloc, Free} family is exposed
   so that you can exploit Python's small-block allocator for non-object
   uses.  If you must use these routines to allocate object memory, make sure
   the object gets initialized via PyObject_{Init, InitVar} after obtaining
   the raw memory.
*/
PyAPI_FUNC(void *) PyObject_Malloc(size_t size);
#if !defined(Py_LIMITED_API) || Py_LIMITED_API+0 >= 0x03050000
PyAPI_FUNC(void *) PyObject_Calloc(size_t nelem, size_t elsize);
#endif
PyAPI_FUNC(void *) PyObject_Realloc(void *ptr, size_t new_size);
PyAPI_FUNC(void) PyObject_Free(void *ptr);

#ifndef Py_LIMITED_API
/* This function returns the number of allocated memory blocks, regardless of size */
PyAPI_FUNC(Py_ssize_t) _Py_GetAllocatedBlocks(void);
#endif /* !Py_LIMITED_API */

/* Macros */
#ifdef WITH_PYMALLOC
#ifndef Py_LIMITED_API
PyAPI_FUNC(int) _PyObject_DebugMallocStats(FILE *out);
#endif /* #ifndef Py_LIMITED_API */
#endif

/* Macros */
#define PyObject_MALLOC         PyObject_Malloc
#define PyObject_REALLOC        PyObject_Realloc
#define PyObject_FREE           PyObject_Free
#define PyObject_Del            PyObject_Free
#define PyObject_DEL            PyObject_Free


/*
 * Generic object allocator interface
 * ==================================
 */

/* Functions */
PyAPI_FUNC(PyObject *) PyObject_Init(PyObject *, PyTypeObject *);
PyAPI_FUNC(PyVarObject *) PyObject_InitVar(PyVarObject *,
                                                 PyTypeObject *, Py_ssize_t);
PyAPI_FUNC(PyObject *) _PyObject_New(PyTypeObject *);
PyAPI_FUNC(PyVarObject *) _PyObject_NewVar(PyTypeObject *, Py_ssize_t);

#define PyObject_New(type, typeobj) \
                ( (type *) _PyObject_New(typeobj) )
#define PyObject_NewVar(type, typeobj, n) \
                ( (type *) _PyObject_NewVar((typeobj), (n)) )

/* Macros trading binary compatibility for speed. See also pymem.h.
   Note that these macros expect non-NULL object pointers.*/
#define PyObject_INIT(op, typeobj) \
    ( Py_TYPE(op) = (typeobj), _Py_NewReference((PyObject *)(op)), (op) )
#define PyObject_INIT_VAR(op, typeobj, size) \
    ( Py_SIZE(op) = (size), PyObject_INIT((op), (typeobj)) )

#define _PyObject_SIZE(typeobj) ( (typeobj)->tp_basicsize )

/* _PyObject_VAR_SIZE returns the number of bytes (as size_t) allocated for a
   vrbl-size object with nitems items, exclusive of gc overhead (if any).  The
   value is rounded up to the closest multiple of sizeof(void *), in order to
   ensure that pointer fields at the end of the object are correctly aligned
   for the platform (this is of special importance for subclasses of, e.g.,
   str or int, so that pointers can be stored after the embedded data).

   Note that there's no memory wastage in doing this, as malloc has to
   return (at worst) pointer-aligned memory anyway.
*/
#if ((SIZEOF_VOID_P - 1) & SIZEOF_VOID_P) != 0
#   error "_PyObject_VAR_SIZE requires SIZEOF_VOID_P be a power of 2"
#endif

#define _PyObject_VAR_SIZE(typeobj, nitems)     \
    _Py_SIZE_ROUND_UP((typeobj)->tp_basicsize + \
        (nitems)*(typeobj)->tp_itemsize,        \
        SIZEOF_VOID_P)

#define PyObject_NEW(type, typeobj) \
( (type *) PyObject_Init( \
    (PyObject *) PyObject_MALLOC( _PyObject_SIZE(typeobj) ), (typeobj)) )

#define PyObject_NEW_VAR(type, typeobj, n) \
( (type *) PyObject_InitVar( \
      (PyVarObject *) PyObject_MALLOC(_PyObject_VAR_SIZE((typeobj),(n)) ),\
      (typeobj), (n)) )

/* This example code implements an object constructor with a custom
   allocator, where PyObject_New is inlined, and shows the important
   distinction between two steps (at least):
       1) the actual allocation of the object storage;
       2) the initialization of the Python specific fields
      in this storage with PyObject_{Init, InitVar}.

   PyObject *
   YourObject_New(...)
   {
       PyObject *op;

       op = (PyObject *) Your_Allocator(_PyObject_SIZE(YourTypeStruct));
       if (op == NULL)
       return PyErr_NoMemory();

       PyObject_Init(op, &YourTypeStruct);

       op->ob_field = value;
       ...
       return op;
   }

   Note that in C++, the use of the new operator usually implies that
   the 1st step is performed automatically for you, so in a C++ class
   constructor you would start directly with PyObject_Init/InitVar
*/

#ifndef Py_LIMITED_API
typedef struct {
    /* user context passed as the first argument to the 2 functions */
    void *ctx;

    /* allocate an arena of size bytes */
    void* (*alloc) (void *ctx, size_t size);

    /* free an arena */
    void (*free) (void *ctx, void *ptr, size_t size);
} PyObjectArenaAllocator;

/* Get the arena allocator. */
PyAPI_FUNC(void) PyObject_GetArenaAllocator(PyObjectArenaAllocator *allocator);

/* Set the arena allocator. */
PyAPI_FUNC(void) PyObject_SetArenaAllocator(PyObjectArenaAllocator *allocator);
#endif


/*
 * Garbage Collection Support
 * ==========================
 */

/* C equivalent of gc.collect() which ignores the state of gc.enabled. */
PyAPI_FUNC(Py_ssize_t) PyGC_Collect(void);

#ifndef Py_LIMITED_API
PyAPI_FUNC(Py_ssize_t) _PyGC_CollectNoFail(void);
PyAPI_FUNC(Py_ssize_t) _PyGC_CollectIfEnabled(void);
#endif

/* Test if a type has a GC head */
#define PyType_IS_GC(t) PyType_HasFeature((t), Py_TPFLAGS_HAVE_GC)

/* Test if an object has a GC head */
#define PyObject_IS_GC(o) (PyType_IS_GC(Py_TYPE(o)) && \
    (Py_TYPE(o)->tp_is_gc == NULL || Py_TYPE(o)->tp_is_gc(o)))

PyAPI_FUNC(PyVarObject *) _PyObject_GC_Resize(PyVarObject *, Py_ssize_t);
#define PyObject_GC_Resize(type, op, n) \
                ( (type *) _PyObject_GC_Resize((PyVarObject *)(op), (n)) )

/* GC information is stored BEFORE the object structure. */
#ifndef Py_LIMITED_API
typedef union _gc_head {
    struct {
        union _gc_head *gc_next;
        union _gc_head *gc_prev;
        Py_ssize_t gc_refs;
    } gc;
    double dummy;  /* force worst-case alignment */
} PyGC_Head;

extern PyGC_Head *_PyGC_generation0;

#define _Py_AS_GC(o) ((PyGC_Head *)(o)-1)

/* Bit 0 is set when tp_finalize is called */
#define _PyGC_REFS_MASK_FINALIZED  (1 << 0)
/* The (N-1) most significant bits contain the gc state / refcount */
#define _PyGC_REFS_SHIFT           (1)
#define _PyGC_REFS_MASK            (((size_t) -1) << _PyGC_REFS_SHIFT)

#define _PyGCHead_REFS(g) ((g)->gc.gc_refs >> _PyGC_REFS_SHIFT)
#define _PyGCHead_SET_REFS(g, v) do { \
    (g)->gc.gc_refs = ((g)->gc.gc_refs & ~_PyGC_REFS_MASK) \
        | (((size_t)(v)) << _PyGC_REFS_SHIFT);             \
    } while (0)
#define _PyGCHead_DECREF(g) ((g)->gc.gc_refs -= 1 << _PyGC_REFS_SHIFT)

#define _PyGCHead_FINALIZED(g) (((g)->gc.gc_refs & _PyGC_REFS_MASK_FINALIZED) != 0)
#define _PyGCHead_SET_FINALIZED(g, v) do {  \
    (g)->gc.gc_refs = ((g)->gc.gc_refs & ~_PyGC_REFS_MASK_FINALIZED) \
        | (v != 0); \
    } while (0)

#define _PyGC_FINALIZED(o) _PyGCHead_FINALIZED(_Py_AS_GC(o))
#define _PyGC_SET_FINALIZED(o, v) _PyGCHead_SET_FINALIZED(_Py_AS_GC(o), v)

#define _PyGC_REFS(o) _PyGCHead_REFS(_Py_AS_GC(o))

#define _PyGC_REFS_UNTRACKED                    (-2)
#define _PyGC_REFS_REACHABLE                    (-3)
#define _PyGC_REFS_TENTATIVELY_UNREACHABLE      (-4)

/* Tell the GC to track this object.  NB: While the object is tracked the
 * collector it must be safe to call the ob_traverse method. */
#define _PyObject_GC_TRACK(o) do { \
    PyGC_Head *g = _Py_AS_GC(o); \
    if (_PyGCHead_REFS(g) != _PyGC_REFS_UNTRACKED) \
        Py_FatalError("GC object already tracked"); \
    _PyGCHead_SET_REFS(g, _PyGC_REFS_REACHABLE); \
    g->gc.gc_next = _PyGC_generation0; \
    g->gc.gc_prev = _PyGC_generation0->gc.gc_prev; \
    g->gc.gc_prev->gc.gc_next = g; \
    _PyGC_generation0->gc.gc_prev = g; \
    } while (0);

/* Tell the GC to stop tracking this object.
 * gc_next doesn't need to be set to NULL, but doing so is a good
 * way to provoke memory errors if calling code is confused.
 */
#define _PyObject_GC_UNTRACK(o) do { \
    PyGC_Head *g = _Py_AS_GC(o); \
    assert(_PyGCHead_REFS(g) != _PyGC_REFS_UNTRACKED); \
    _PyGCHead_SET_REFS(g, _PyGC_REFS_UNTRACKED); \
    g->gc.gc_prev->gc.gc_next = g->gc.gc_next; \
    g->gc.gc_next->gc.gc_prev = g->gc.gc_prev; \
    g->gc.gc_next = NULL; \
    } while (0);

/* True if the object is currently tracked by the GC. */
#define _PyObject_GC_IS_TRACKED(o) \
    (_PyGC_REFS(o) != _PyGC_REFS_UNTRACKED)

/* True if the object may be tracked by the GC in the future, or already is.
   This can be useful to implement some optimizations. */
#define _PyObject_GC_MAY_BE_TRACKED(obj) \
    (PyObject_IS_GC(obj) && \
        (!PyTuple_CheckExact(obj) || _PyObject_GC_IS_TRACKED(obj)))
#endif /* Py_LIMITED_API */

#ifndef Py_LIMITED_API
PyAPI_FUNC(PyObject *) _PyObject_GC_Malloc(size_t size);
PyAPI_FUNC(PyObject *) _PyObject_GC_Calloc(size_t size);
#endif /* !Py_LIMITED_API */
PyAPI_FUNC(PyObject *) _PyObject_GC_New(PyTypeObject *);
PyAPI_FUNC(PyVarObject *) _PyObject_GC_NewVar(PyTypeObject *, Py_ssize_t);
PyAPI_FUNC(void) PyObject_GC_Track(void *);
PyAPI_FUNC(void) PyObject_GC_UnTrack(void *);
PyAPI_FUNC(void) PyObject_GC_Del(void *);

#define PyObject_GC_New(type, typeobj) \
                ( (type *) _PyObject_GC_New(typeobj) )
#define PyObject_GC_NewVar(type, typeobj, n) \
                ( (type *) _PyObject_GC_NewVar((typeobj), (n)) )


/* Utility macro to help write tp_traverse functions.
 * To use this macro, the tp_traverse function must name its arguments
 * "visit" and "arg".  This is intended to keep tp_traverse functions
 * looking as much alike as possible.
 */
#define Py_VISIT(op)                                                    \
    do {                                                                \
        if (op) {                                                       \
            int vret = visit((PyObject *)(op), arg);                    \
            if (vret)                                                   \
                return vret;                                            \
        }                                                               \
    } while (0)


/* Test if a type supports weak references */
#define PyType_SUPPORTS_WEAKREFS(t) ((t)->tp_weaklistoffset > 0)

#define PyObject_GET_WEAKREFS_LISTPTR(o) \
    ((PyObject **) (((char *) (o)) + Py_TYPE(o)->tp_weaklistoffset))

#ifdef __cplusplus
}
#endif
#endif /* !Py_OBJIMPL_H */
