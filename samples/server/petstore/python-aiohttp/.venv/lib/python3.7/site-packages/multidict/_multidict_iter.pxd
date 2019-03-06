from cpython.object cimport PyObject

cdef extern from "_multidict_iter.h":

    object multidict_items_iter_new(object impl)
    object multidict_keys_iter_new(object impl)
    object multidict_values_iter_new(object impl)

    int multidict_iter_init() except -1
