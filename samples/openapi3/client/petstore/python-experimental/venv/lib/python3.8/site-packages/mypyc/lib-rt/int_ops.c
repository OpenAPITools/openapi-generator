// Int primitive operations
//
// These are registered in mypyc.primitives.int_ops.

#include <Python.h>
#include "CPy.h"

CPyTagged CPyTagged_FromSsize_t(Py_ssize_t value) {
    // We use a Python object if the value shifted left by 1 is too
    // large for Py_ssize_t
    if (CPyTagged_TooBig(value)) {
        PyObject *object = PyLong_FromSsize_t(value);
        return ((CPyTagged)object) | CPY_INT_TAG;
    } else {
        return value << 1;
    }
}

CPyTagged CPyTagged_FromObject(PyObject *object) {
    int overflow;
    // The overflow check knows about CPyTagged's width
    Py_ssize_t value = CPyLong_AsSsize_tAndOverflow(object, &overflow);
    if (overflow != 0) {
        Py_INCREF(object);
        return ((CPyTagged)object) | CPY_INT_TAG;
    } else {
        return value << 1;
    }
}

CPyTagged CPyTagged_StealFromObject(PyObject *object) {
    int overflow;
    // The overflow check knows about CPyTagged's width
    Py_ssize_t value = CPyLong_AsSsize_tAndOverflow(object, &overflow);
    if (overflow != 0) {
        return ((CPyTagged)object) | CPY_INT_TAG;
    } else {
        Py_DECREF(object);
        return value << 1;
    }
}

CPyTagged CPyTagged_BorrowFromObject(PyObject *object) {
    int overflow;
    // The overflow check knows about CPyTagged's width
    Py_ssize_t value = CPyLong_AsSsize_tAndOverflow(object, &overflow);
    if (overflow != 0) {
        return ((CPyTagged)object) | CPY_INT_TAG;
    } else {
        return value << 1;
    }
}

PyObject *CPyTagged_AsObject(CPyTagged x) {
    PyObject *value;
    if (CPyTagged_CheckLong(x)) {
        value = CPyTagged_LongAsObject(x);
        Py_INCREF(value);
    } else {
        value = PyLong_FromSsize_t(CPyTagged_ShortAsSsize_t(x));
        if (value == NULL) {
            CPyError_OutOfMemory();
        }
    }
    return value;
}

PyObject *CPyTagged_StealAsObject(CPyTagged x) {
    PyObject *value;
    if (CPyTagged_CheckLong(x)) {
        value = CPyTagged_LongAsObject(x);
    } else {
        value = PyLong_FromSsize_t(CPyTagged_ShortAsSsize_t(x));
        if (value == NULL) {
            CPyError_OutOfMemory();
        }
    }
    return value;
}

Py_ssize_t CPyTagged_AsSsize_t(CPyTagged x) {
    if (CPyTagged_CheckShort(x)) {
        return CPyTagged_ShortAsSsize_t(x);
    } else {
        return PyLong_AsSsize_t(CPyTagged_LongAsObject(x));
    }
}

CPy_NOINLINE
void CPyTagged_IncRef(CPyTagged x) {
    if (CPyTagged_CheckLong(x)) {
        Py_INCREF(CPyTagged_LongAsObject(x));
    }
}

CPy_NOINLINE
void CPyTagged_DecRef(CPyTagged x) {
    if (CPyTagged_CheckLong(x)) {
        Py_DECREF(CPyTagged_LongAsObject(x));
    }
}

CPy_NOINLINE
void CPyTagged_XDecRef(CPyTagged x) {
    if (CPyTagged_CheckLong(x)) {
        Py_XDECREF(CPyTagged_LongAsObject(x));
    }
}

CPyTagged CPyTagged_Negate(CPyTagged num) {
    if (CPyTagged_CheckShort(num)
            && num != (CPyTagged) ((Py_ssize_t)1 << (CPY_INT_BITS - 1))) {
        // The only possibility of an overflow error happening when negating a short is if we
        // attempt to negate the most negative number.
        return -num;
    }
    PyObject *num_obj = CPyTagged_AsObject(num);
    PyObject *result = PyNumber_Negative(num_obj);
    if (result == NULL) {
        CPyError_OutOfMemory();
    }
    Py_DECREF(num_obj);
    return CPyTagged_StealFromObject(result);
}

CPyTagged CPyTagged_Add(CPyTagged left, CPyTagged right) {
    // TODO: Use clang/gcc extension __builtin_saddll_overflow instead.
    if (CPyTagged_CheckShort(left) && CPyTagged_CheckShort(right)) {
        CPyTagged sum = left + right;
        if (!CPyTagged_IsAddOverflow(sum, left, right)) {
            return sum;
        }
    }
    PyObject *left_obj = CPyTagged_AsObject(left);
    PyObject *right_obj = CPyTagged_AsObject(right);
    PyObject *result = PyNumber_Add(left_obj, right_obj);
    if (result == NULL) {
        CPyError_OutOfMemory();
    }
    Py_DECREF(left_obj);
    Py_DECREF(right_obj);
    return CPyTagged_StealFromObject(result);
}

CPyTagged CPyTagged_Subtract(CPyTagged left, CPyTagged right) {
    // TODO: Use clang/gcc extension __builtin_saddll_overflow instead.
    if (CPyTagged_CheckShort(left) && CPyTagged_CheckShort(right)) {
        CPyTagged diff = left - right;
        if (!CPyTagged_IsSubtractOverflow(diff, left, right)) {
            return diff;
        }
    }
    PyObject *left_obj = CPyTagged_AsObject(left);
    PyObject *right_obj = CPyTagged_AsObject(right);
    PyObject *result = PyNumber_Subtract(left_obj, right_obj);
    if (result == NULL) {
        CPyError_OutOfMemory();
    }
    Py_DECREF(left_obj);
    Py_DECREF(right_obj);
    return CPyTagged_StealFromObject(result);
}

CPyTagged CPyTagged_Multiply(CPyTagged left, CPyTagged right) {
    // TODO: Consider using some clang/gcc extension
    if (CPyTagged_CheckShort(left) && CPyTagged_CheckShort(right)) {
        if (!CPyTagged_IsMultiplyOverflow(left, right)) {
            return left * CPyTagged_ShortAsSsize_t(right);
        }
    }
    PyObject *left_obj = CPyTagged_AsObject(left);
    PyObject *right_obj = CPyTagged_AsObject(right);
    PyObject *result = PyNumber_Multiply(left_obj, right_obj);
    if (result == NULL) {
        CPyError_OutOfMemory();
    }
    Py_DECREF(left_obj);
    Py_DECREF(right_obj);
    return CPyTagged_StealFromObject(result);
}

CPyTagged CPyTagged_FloorDivide(CPyTagged left, CPyTagged right) {
    if (CPyTagged_CheckShort(left) && CPyTagged_CheckShort(right)
        && !CPyTagged_MaybeFloorDivideFault(left, right)) {
        Py_ssize_t result = ((Py_ssize_t)left / CPyTagged_ShortAsSsize_t(right)) & ~1;
        if (((Py_ssize_t)left < 0) != (((Py_ssize_t)right) < 0)) {
            if (result / 2 * right != left) {
                // Round down
                result -= 2;
            }
        }
        return result;
    }
    PyObject *left_obj = CPyTagged_AsObject(left);
    PyObject *right_obj = CPyTagged_AsObject(right);
    PyObject *result = PyNumber_FloorDivide(left_obj, right_obj);
    Py_DECREF(left_obj);
    Py_DECREF(right_obj);
    // Handle exceptions honestly because it could be ZeroDivisionError
    if (result == NULL) {
        return CPY_INT_TAG;
    } else {
        return CPyTagged_StealFromObject(result);
    }
}

CPyTagged CPyTagged_Remainder(CPyTagged left, CPyTagged right) {
    if (CPyTagged_CheckShort(left) && CPyTagged_CheckShort(right)
        && !CPyTagged_MaybeRemainderFault(left, right)) {
        Py_ssize_t result = (Py_ssize_t)left % (Py_ssize_t)right;
        if (((Py_ssize_t)right < 0) != ((Py_ssize_t)left < 0) && result != 0) {
            result += right;
        }
        return result;
    }
    PyObject *left_obj = CPyTagged_AsObject(left);
    PyObject *right_obj = CPyTagged_AsObject(right);
    PyObject *result = PyNumber_Remainder(left_obj, right_obj);
    Py_DECREF(left_obj);
    Py_DECREF(right_obj);
    // Handle exceptions honestly because it could be ZeroDivisionError
    if (result == NULL) {
        return CPY_INT_TAG;
    } else {
        return CPyTagged_StealFromObject(result);
    }
}

bool CPyTagged_IsEq_(CPyTagged left, CPyTagged right) {
    if (CPyTagged_CheckShort(right)) {
        return false;
    } else {
        int result = PyObject_RichCompareBool(CPyTagged_LongAsObject(left),
                                              CPyTagged_LongAsObject(right), Py_EQ);
        if (result == -1) {
            CPyError_OutOfMemory();
        }
        return result;
    }
}

bool CPyTagged_IsLt_(CPyTagged left, CPyTagged right) {
    PyObject *left_obj = CPyTagged_AsObject(left);
    PyObject *right_obj = CPyTagged_AsObject(right);
    int result = PyObject_RichCompareBool(left_obj, right_obj, Py_LT);
    Py_DECREF(left_obj);
    Py_DECREF(right_obj);
    if (result == -1) {
        CPyError_OutOfMemory();
    }
    return result;
}

PyObject *CPyLong_FromStrWithBase(PyObject *o, CPyTagged base) {
    Py_ssize_t base_size_t = CPyTagged_AsSsize_t(base);
    return PyLong_FromUnicodeObject(o, base_size_t);
}

PyObject *CPyLong_FromStr(PyObject *o) {
    CPyTagged base = CPyTagged_FromSsize_t(10);
    return CPyLong_FromStrWithBase(o, base);
}

PyObject *CPyLong_FromFloat(PyObject *o) {
    if (PyLong_Check(o)) {
        CPy_INCREF(o);
        return o;
    } else {
        return PyLong_FromDouble(PyFloat_AS_DOUBLE(o));
    }
}

PyObject *CPyBool_Str(bool b) {
    return PyObject_Str(b ? Py_True : Py_False);
}

static void CPyLong_NormalizeUnsigned(PyLongObject *v) {
    Py_ssize_t i = v->ob_base.ob_size;
    while (i > 0 && v->ob_digit[i - 1] == 0)
        i--;
    v->ob_base.ob_size = i;
}

// Bitwise op '&', '|' or '^' using the generic (slow) API
static CPyTagged GenericBitwiseOp(CPyTagged a, CPyTagged b, char op) {
    PyObject *aobj = CPyTagged_AsObject(a);
    PyObject *bobj = CPyTagged_AsObject(b);
    PyObject *r;
    if (op == '&') {
        r = PyNumber_And(aobj, bobj);
    } else if (op == '|') {
        r = PyNumber_Or(aobj, bobj);
    } else {
        r = PyNumber_Xor(aobj, bobj);
    }
    if (unlikely(r == NULL)) {
        CPyError_OutOfMemory();
    }
    Py_DECREF(aobj);
    Py_DECREF(bobj);
    return CPyTagged_StealFromObject(r);
}

// Return pointer to digits of a PyLong object. If it's a short
// integer, place digits in the buffer buf instead to avoid memory
// allocation (it's assumed to be big enough). Return the number of
// digits in *size. *size is negative if the integer is negative.
static digit *GetIntDigits(CPyTagged n, Py_ssize_t *size, digit *buf) {
    if (CPyTagged_CheckShort(n)) {
        Py_ssize_t val = CPyTagged_ShortAsSsize_t(n);
        bool neg = val < 0;
        int len = 1;
        if (neg) {
            val = -val;
        }
        buf[0] = val & PyLong_MASK;
        if (val > PyLong_MASK) {
            val >>= PyLong_SHIFT;
            buf[1] = val & PyLong_MASK;
            if (val > PyLong_MASK) {
                buf[2] = val >> PyLong_SHIFT;
                len = 3;
            } else {
                len = 2;
            }
        }
        *size = neg ? -len : len;
        return buf;
    } else {
        PyLongObject *obj = (PyLongObject *)CPyTagged_LongAsObject(n);
        *size = obj->ob_base.ob_size;
        return obj->ob_digit;
    }
}

// Shared implementation of bitwise '&', '|' and '^' (specified by op) for at least
// one long operand. This is somewhat optimized for performance.
static CPyTagged BitwiseLongOp(CPyTagged a, CPyTagged b, char op) {
    // Directly access the digits, as there is no fast C API function for this.
    digit abuf[3];
    digit bbuf[3];
    Py_ssize_t asize;
    Py_ssize_t bsize;
    digit *adigits = GetIntDigits(a, &asize, abuf);
    digit *bdigits = GetIntDigits(b, &bsize, bbuf);

    PyLongObject *r;
    if (unlikely(asize < 0 || bsize < 0)) {
        // Negative operand. This is slower, but bitwise ops on them are pretty rare.
        return GenericBitwiseOp(a, b, op);
    }
    // Optimized implementation for two non-negative integers.
    // Swap a and b as needed to ensure a is no longer than b.
    if (asize > bsize) {
        digit *tmp = adigits;
        adigits = bdigits;
        bdigits = tmp;
        Py_ssize_t tmp_size = asize;
        asize = bsize;
        bsize = tmp_size;
    }
    r = _PyLong_New(op == '&' ? asize : bsize);
    if (unlikely(r == NULL)) {
        CPyError_OutOfMemory();
    }
    Py_ssize_t i;
    if (op == '&') {
        for (i = 0; i < asize; i++) {
            r->ob_digit[i] = adigits[i] & bdigits[i];
        }
    } else {
        if (op == '|') {
            for (i = 0; i < asize; i++) {
                r->ob_digit[i] = adigits[i] | bdigits[i];
            }
        } else {
            for (i = 0; i < asize; i++) {
                r->ob_digit[i] = adigits[i] ^ bdigits[i];
            }
        }
        for (; i < bsize; i++) {
            r->ob_digit[i] = bdigits[i];
        }
    }
    CPyLong_NormalizeUnsigned(r);
    return CPyTagged_StealFromObject((PyObject *)r);
}

// Bitwise '&'
CPyTagged CPyTagged_And(CPyTagged left, CPyTagged right) {
    if (likely(CPyTagged_CheckShort(left) && CPyTagged_CheckShort(right))) {
        return left & right;
    }
    return BitwiseLongOp(left, right, '&');
}

// Bitwise '|'
CPyTagged CPyTagged_Or(CPyTagged left, CPyTagged right) {
    if (likely(CPyTagged_CheckShort(left) && CPyTagged_CheckShort(right))) {
        return left | right;
    }
    return BitwiseLongOp(left, right, '|');
}

// Bitwise '^'
CPyTagged CPyTagged_Xor(CPyTagged left, CPyTagged right) {
    if (likely(CPyTagged_CheckShort(left) && CPyTagged_CheckShort(right))) {
        return left ^ right;
    }
    return BitwiseLongOp(left, right, '^');
}

// Bitwise '~'
CPyTagged CPyTagged_Invert(CPyTagged num) {
    if (likely(CPyTagged_CheckShort(num) && num != CPY_TAGGED_ABS_MIN)) {
        return ~num & ~CPY_INT_TAG;
    } else {
        PyObject *obj = CPyTagged_AsObject(num);
        PyObject *result = PyNumber_Invert(obj);
        if (unlikely(result == NULL)) {
            CPyError_OutOfMemory();
        }
        Py_DECREF(obj);
        return CPyTagged_StealFromObject(result);
    }
}

// Bitwise '>>'
CPyTagged CPyTagged_Rshift(CPyTagged left, CPyTagged right) {
    if (likely(CPyTagged_CheckShort(left)
               && CPyTagged_CheckShort(right)
               && (Py_ssize_t)right >= 0)) {
        CPyTagged count = CPyTagged_ShortAsSsize_t(right);
        if (unlikely(count >= CPY_INT_BITS)) {
            if ((Py_ssize_t)left >= 0) {
                return 0;
            } else {
                return CPyTagged_ShortFromInt(-1);
            }
        }
        return ((Py_ssize_t)left >> count) & ~CPY_INT_TAG;
    } else {
        // Long integer or negative shift -- use generic op
        PyObject *lobj = CPyTagged_AsObject(left);
        PyObject *robj = CPyTagged_AsObject(right);
        PyObject *result = PyNumber_Rshift(lobj, robj);
        Py_DECREF(lobj);
        Py_DECREF(robj);
        if (result == NULL) {
            // Propagate error (could be negative shift count)
            return CPY_INT_TAG;
        }
        return CPyTagged_StealFromObject(result);
    }
}

static inline bool IsShortLshiftOverflow(Py_ssize_t short_int, Py_ssize_t shift) {
    return ((Py_ssize_t)(short_int << shift) >> shift) != short_int;
}

// Bitwise '<<'
CPyTagged CPyTagged_Lshift(CPyTagged left, CPyTagged right) {
    if (likely(CPyTagged_CheckShort(left)
               && CPyTagged_CheckShort(right)
               && (Py_ssize_t)right >= 0
               && right < CPY_INT_BITS * 2)) {
        CPyTagged shift = CPyTagged_ShortAsSsize_t(right);
        if (!IsShortLshiftOverflow(left, shift))
            // Short integers, no overflow
            return left << shift;
    }
    // Long integer or out of range shift -- use generic op
    PyObject *lobj = CPyTagged_AsObject(left);
    PyObject *robj = CPyTagged_AsObject(right);
    PyObject *result = PyNumber_Lshift(lobj, robj);
    Py_DECREF(lobj);
    Py_DECREF(robj);
    if (result == NULL) {
        // Propagate error (could be negative shift count)
        return CPY_INT_TAG;
    }
    return CPyTagged_StealFromObject(result);
}
