#import "AnyPromise.h"
#import "AnyPromise+Private.h"
@import Foundation.NSKeyValueCoding;
#import "PMKCallVariadicBlock.m"

NSString *const PMKErrorDomain = @"PMKErrorDomain";


@implementation AnyPromise (objc)

- (instancetype)initWithResolver:(PMKResolver __strong *)resolver {
    return [self initWithBridge:^(PMKResolver resolve){
        *resolver = resolve;
    }];
}

+ (instancetype)promiseWithResolverBlock:(void (^)(PMKResolver))resolveBlock {
    return [[self alloc] initWithBridge:resolveBlock];
}

+ (instancetype)promiseWithValue:(id)value {
    return [[self alloc] initWithBridge:^(PMKResolver resolve){
        resolve(value);
    }];
}

static inline AnyPromise *AnyPromiseWhen(AnyPromise *when, void(^then)(id, PMKResolver)) {
    return [[AnyPromise alloc] initWithBridge:^(PMKResolver resolve){
        [when pipe:^(id obj){
            then(obj, resolve);
        }];
    }];
}

static inline AnyPromise *__then(AnyPromise *self, dispatch_queue_t queue, id block) {
    return AnyPromiseWhen(self, ^(id obj, PMKResolver resolve) {
        if (IsError(obj)) {
            resolve(obj);
        } else dispatch_async(queue, ^{
            resolve(PMKCallVariadicBlock(block, obj));
        });
    });
}

- (AnyPromise *(^)(id))then {
    return ^(id block) {
        return __then(self, dispatch_get_main_queue(), block);
    };
}

- (AnyPromise *(^)(dispatch_queue_t, id))thenOn {
    return ^(dispatch_queue_t queue, id block) {
        return __then(self, queue, block);
    };
}

- (AnyPromise *(^)(id))thenInBackground {
    return ^(id block) {
        return __then(self, dispatch_get_global_queue(0, 0), block);
    };
}

static inline AnyPromise *__catch(AnyPromise *self, BOOL includeCancellation, id block) {
    return AnyPromiseWhen(self, ^(id obj, PMKResolver resolve) {
        dispatch_async(dispatch_get_main_queue(), ^{
            if (IsError(obj) && (includeCancellation || ![obj cancelled])) {
                [obj pmk_consume];
                resolve(PMKCallVariadicBlock(block, obj));
            } else {
                resolve(obj);
            }
        });
    });
}

- (AnyPromise *(^)(id))catch {
    return ^(id block) {
        return __catch(self, NO, block);
    };
}

- (AnyPromise *(^)(PMKCatchPolicy, id))catchWithPolicy {
    return ^(PMKCatchPolicy policy, id block) {
        return __catch(self, policy == PMKCatchPolicyAllErrors, block);
    };
}

static inline AnyPromise *__finally(AnyPromise *self, dispatch_queue_t queue, dispatch_block_t block) {
    return AnyPromiseWhen(self, ^(id obj, PMKResolver resolve) {
        dispatch_async(queue, ^{
            block();
            resolve(obj);
        });
    });
}

- (AnyPromise *(^)(dispatch_block_t))finally {
    return ^(dispatch_block_t block) {
        return __finally(self, dispatch_get_main_queue(), block);
    };
}

- (AnyPromise *(^)(dispatch_queue_t, dispatch_block_t))finallyOn {
    return ^(dispatch_queue_t queue, dispatch_block_t block) {
        return __finally(self, queue, block);
    };
}

- (id)value {
    id result = [self valueForKey:@"__value"];
    return [result isKindOfClass:[PMKArray class]]
        ? result[0]
        : result;
}

@end



@implementation AnyPromise (Adapters)

+ (instancetype)promiseWithAdapterBlock:(void (^)(PMKAdapter))block {
    return [self promiseWithResolverBlock:^(PMKResolver resolve) {
        block(^(id value, id error){
            resolve(error ?: value);
        });
    }];
}

+ (instancetype)promiseWithIntegerAdapterBlock:(void (^)(PMKIntegerAdapter))block {
    return [self promiseWithResolverBlock:^(PMKResolver resolve) {
        block(^(NSInteger value, id error){
            if (error) {
                resolve(error);
            } else {
                resolve(@(value));
            }
        });
    }];
}

+ (instancetype)promiseWithBooleanAdapterBlock:(void (^)(PMKBooleanAdapter adapter))block {
    return [self promiseWithResolverBlock:^(PMKResolver resolve) {
        block(^(BOOL value, id error){
            if (error) {
                resolve(error);
            } else {
                resolve(@(value));
            }
        });
    }];
}

@end
