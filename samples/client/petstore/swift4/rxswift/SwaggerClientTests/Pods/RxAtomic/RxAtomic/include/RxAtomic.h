//
//  RxAtomic.h
//  RxAtomic
//
//  Created by Krunoslav Zaher on 10/28/18.
//  Copyright © 2018 Krunoslav Zaher. All rights reserved.
//

#ifndef RxAtomic_h
#define RxAtomic_h

#include <stdatomic.h>

#define SWIFT_NAME(_name) __attribute__((swift_name(#_name)))

#define Atomic(swift_type, llvm_type) \
    typedef struct { volatile atomic_##llvm_type atom; } Atomic##swift_type;\
    static __inline__ __attribute__((__always_inline__)) SWIFT_NAME(Atomic##swift_type.initialize(self:_:)) \
    void Atomic##swift_type##_Initialize(Atomic##swift_type * _Nonnull self, llvm_type value) { \
        atomic_init(&self->atom, value);\
    }\
    \
    static __inline__ __attribute__((__always_inline__)) SWIFT_NAME(Atomic##swift_type.load(self:)) \
    llvm_type Atomic##swift_type##_Load(Atomic##swift_type * _Nonnull self) { \
        return atomic_load(&self->atom);\
    }\
    \
    static __inline__ __attribute__((__always_inline__)) SWIFT_NAME(Atomic##swift_type.fetchOr(self:_:)) \
    llvm_type Atomic##swift_type##_FetchOr(Atomic##swift_type * _Nonnull self, llvm_type mask) { \
        return atomic_fetch_or(&self->atom, mask);\
    }\
    \
    static __inline__ __attribute__((__always_inline__)) SWIFT_NAME(Atomic##swift_type.add(self:_:)) \
    llvm_type Atomic##swift_type##_Add(Atomic##swift_type * _Nonnull self, llvm_type value) { \
        return atomic_fetch_add(&self->atom, value);\
    }\
    \
    static __inline__ __attribute__((__always_inline__)) SWIFT_NAME(Atomic##swift_type.sub(self:_:)) \
    llvm_type Atomic##swift_type##_Sub(Atomic##swift_type * _Nonnull self, llvm_type value) { \
        return atomic_fetch_sub(&self->atom, value);\
    }\
    \

Atomic(Int, int)

#undef SWIFT_NAME

#endif /* RxAtomic_h */
