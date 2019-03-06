from cpython.object cimport PyObject

cdef extern from "_multidict_views.h":

    object multidict_itemsview_new(object impl)
    object multidict_keysview_new(object impl)
    object multidict_valuesview_new(object impl)

    int multidict_views_init() except -1
