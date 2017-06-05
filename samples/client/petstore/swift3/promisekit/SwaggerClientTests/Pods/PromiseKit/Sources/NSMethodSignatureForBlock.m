#import <Foundation/NSMethodSignature.h>

struct PMKBlockLiteral {
    void *isa; // initialized to &_NSConcreteStackBlock or &_NSConcreteGlobalBlock
    int flags;
    int reserved;
    void (*invoke)(void *, ...);
    struct block_descriptor {
        unsigned long int reserved;	// NULL
    	unsigned long int size;         // sizeof(struct Block_literal_1)
        // optional helper functions
    	void (*copy_helper)(void *dst, void *src);     // IFF (1<<25)
    	void (*dispose_helper)(void *src);             // IFF (1<<25)
        // required ABI.2010.3.16
        const char *signature;                         // IFF (1<<30)
    } *descriptor;
    // imported variables
};

typedef NS_OPTIONS(NSUInteger, PMKBlockDescriptionFlags) {
    PMKBlockDescriptionFlagsHasCopyDispose = (1 << 25),
    PMKBlockDescriptionFlagsHasCtor = (1 << 26), // helpers have C++ code
    PMKBlockDescriptionFlagsIsGlobal = (1 << 28),
    PMKBlockDescriptionFlagsHasStret = (1 << 29), // IFF BLOCK_HAS_SIGNATURE
    PMKBlockDescriptionFlagsHasSignature = (1 << 30)
};

// It appears 10.7 doesn't support quotes in method signatures. Remove them
// via @rabovik's method. See https://github.com/OliverLetterer/SLObjectiveCRuntimeAdditions/pull/2
#if defined(__MAC_OS_X_VERSION_MIN_REQUIRED) && __MAC_OS_X_VERSION_MIN_REQUIRED < __MAC_10_8
NS_INLINE static const char * pmk_removeQuotesFromMethodSignature(const char *str){
    char *result = malloc(strlen(str) + 1);
    BOOL skip = NO;
    char *to = result;
    char c;
    while ((c = *str++)) {
        if ('"' == c) {
            skip = !skip;
            continue;
        }
        if (skip) continue;
        *to++ = c;
    }
    *to = '\0';
    return result;
}
#endif

static NSMethodSignature *NSMethodSignatureForBlock(id block) {
    if (!block)
        return nil;

    struct PMKBlockLiteral *blockRef = (__bridge struct PMKBlockLiteral *)block;
    PMKBlockDescriptionFlags flags = (PMKBlockDescriptionFlags)blockRef->flags;

    if (flags & PMKBlockDescriptionFlagsHasSignature) {
        void *signatureLocation = blockRef->descriptor;
        signatureLocation += sizeof(unsigned long int);
        signatureLocation += sizeof(unsigned long int);

        if (flags & PMKBlockDescriptionFlagsHasCopyDispose) {
            signatureLocation += sizeof(void(*)(void *dst, void *src));
            signatureLocation += sizeof(void (*)(void *src));
        }

        const char *signature = (*(const char **)signatureLocation);
#if defined(__MAC_OS_X_VERSION_MIN_REQUIRED) && __MAC_OS_X_VERSION_MIN_REQUIRED < __MAC_10_8
        signature = pmk_removeQuotesFromMethodSignature(signature);
        NSMethodSignature *nsSignature = [NSMethodSignature signatureWithObjCTypes:signature];
        free((void *)signature);

        return nsSignature;
#endif
        return [NSMethodSignature signatureWithObjCTypes:signature];
    }
    return 0;
}
