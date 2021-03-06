"""List primitive ops."""

from mypyc.ir.ops import ERR_MAGIC, ERR_NEVER, ERR_FALSE
from mypyc.ir.rtypes import (
    int_rprimitive, short_int_rprimitive, list_rprimitive, object_rprimitive,  c_int_rprimitive,
    c_pyssize_t_rprimitive, bit_rprimitive
)
from mypyc.primitives.registry import (
    load_address_op, function_op, binary_op, method_op, custom_op, ERR_NEG_INT
)


# Get the 'builtins.list' type object.
load_address_op(
    name='builtins.list',
    type=object_rprimitive,
    src='PyList_Type')

# list(obj)
to_list = function_op(
    name='builtins.list',
    arg_types=[object_rprimitive],
    return_type=list_rprimitive,
    c_function_name='PySequence_List',
    error_kind=ERR_MAGIC,
)

new_list_op = custom_op(
    arg_types=[c_pyssize_t_rprimitive],
    return_type=list_rprimitive,
    c_function_name='PyList_New',
    error_kind=ERR_MAGIC)

# list[index] (for an integer index)
list_get_item_op = method_op(
    name='__getitem__',
    arg_types=[list_rprimitive, int_rprimitive],
    return_type=object_rprimitive,
    c_function_name='CPyList_GetItem',
    error_kind=ERR_MAGIC)

# Version with no int bounds check for when it is known to be short
method_op(
    name='__getitem__',
    arg_types=[list_rprimitive, short_int_rprimitive],
    return_type=object_rprimitive,
    c_function_name='CPyList_GetItemShort',
    error_kind=ERR_MAGIC,
    priority=2)

# This is unsafe because it assumes that the index is a non-negative short integer
# that is in-bounds for the list.
list_get_item_unsafe_op = custom_op(
    arg_types=[list_rprimitive, short_int_rprimitive],
    return_type=object_rprimitive,
    c_function_name='CPyList_GetItemUnsafe',
    error_kind=ERR_NEVER)

# list[index] = obj
list_set_item_op = method_op(
    name='__setitem__',
    arg_types=[list_rprimitive, int_rprimitive, object_rprimitive],
    return_type=bit_rprimitive,
    c_function_name='CPyList_SetItem',
    error_kind=ERR_FALSE,
    steals=[False, False, True])

# list.append(obj)
list_append_op = method_op(
    name='append',
    arg_types=[list_rprimitive, object_rprimitive],
    return_type=c_int_rprimitive,
    c_function_name='PyList_Append',
    error_kind=ERR_NEG_INT)

# list.extend(obj)
list_extend_op = method_op(
    name='extend',
    arg_types=[list_rprimitive, object_rprimitive],
    return_type=object_rprimitive,
    c_function_name='CPyList_Extend',
    error_kind=ERR_MAGIC)

# list.pop()
list_pop_last = method_op(
    name='pop',
    arg_types=[list_rprimitive],
    return_type=object_rprimitive,
    c_function_name='CPyList_PopLast',
    error_kind=ERR_MAGIC)

# list.pop(index)
list_pop = method_op(
    name='pop',
    arg_types=[list_rprimitive, int_rprimitive],
    return_type=object_rprimitive,
    c_function_name='CPyList_Pop',
    error_kind=ERR_MAGIC)

# list.count(obj)
method_op(
    name='count',
    arg_types=[list_rprimitive, object_rprimitive],
    return_type=short_int_rprimitive,
    c_function_name='CPyList_Count',
    error_kind=ERR_MAGIC)

# list.insert(index, obj)
method_op(
    name='insert',
    arg_types=[list_rprimitive, int_rprimitive, object_rprimitive],
    return_type=c_int_rprimitive,
    c_function_name='CPyList_Insert',
    error_kind=ERR_NEG_INT)

# list * int
binary_op(
    name='*',
    arg_types=[list_rprimitive, int_rprimitive],
    return_type=list_rprimitive,
    c_function_name='CPySequence_Multiply',
    error_kind=ERR_MAGIC)

# int * list
binary_op(name='*',
          arg_types=[int_rprimitive, list_rprimitive],
          return_type=list_rprimitive,
          c_function_name='CPySequence_RMultiply',
          error_kind=ERR_MAGIC)

# list[begin:end]
list_slice_op = custom_op(
    arg_types=[list_rprimitive, int_rprimitive, int_rprimitive],
    return_type=object_rprimitive,
    c_function_name='CPyList_GetSlice',
    error_kind=ERR_MAGIC,)
