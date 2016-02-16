#import <dispatch/object.h>
#import <dispatch/queue.h>
#import <Foundation/NSObject.h>
#import "Umbrella.h"

typedef void (^PMKResolver)(id __nullable);

typedef NS_ENUM(NSInteger, PMKCatchPolicy) {
    PMKCatchPolicyAllErrors,
    PMKCatchPolicyAllErrorsExceptCancellation
};


/**
 @see AnyPromise.swift
*/
@interface AnyPromise (objc)

/**
 The provided block is executed when its receiver is resolved.

 If you provide a block that takes a parameter, the value of the receiver will be passed as that parameter.

 @param block The block that is executed when the receiver is resolved.

    [NSURLConnection GET:url].then(^(NSData *data){
        // do something with data
    });

 @return A new promise that is resolved with the value returned from the provided block. For example:

    [NSURLConnection GET:url].then(^(NSData *data){
        return data.length;
    }).then(^(NSNumber *number){
        //…
    });

 @warning *Important* The block passed to `then` may take zero, one, two or three arguments, and return an object or return nothing. This flexibility is why the method signature for then is `id`, which means you will not get completion for the block parameter, and must type it yourself. It is safe to type any block syntax here, so to start with try just: `^{}`.

 @warning *Important* If an exception is thrown inside your block, or you return an `NSError` object the next `Promise` will be rejected. See `catch` for documentation on error handling.

 @warning *Important* `then` is always executed on the main queue.

 @see thenOn
 @see thenInBackground
*/
- (AnyPromise * __nonnull (^ __nonnull)(id __nonnull))then;


/**
 The provided block is executed on the default queue when the receiver is fulfilled.

 This method is provided as a convenience for `thenOn`.

 @see then
 @see thenOn
*/
- (AnyPromise * __nonnull(^ __nonnull)(id __nonnull))thenInBackground;

/**
 The provided block is executed on the dispatch queue of your choice when the receiver is fulfilled.

 @see then
 @see thenInBackground
*/
- (AnyPromise * __nonnull(^ __nonnull)(dispatch_queue_t __nonnull, id __nonnull))thenOn;

#ifndef __cplusplus
/**
 The provided block is executed when the receiver is rejected.

 Provide a block of form `^(NSError *){}` or simply `^{}`. The parameter has type `id` to give you the freedom to choose either.

 The provided block always runs on the main queue.
 
 @warning *Note* Cancellation errors are not caught.
 
 @warning *Note* Since catch is a c++ keyword, this method is not availble in Objective-C++ files. Instead use catchWithPolicy.

 @see catchWithPolicy
*/
- (AnyPromise * __nonnull(^ __nonnull)(id __nonnull))catch;
#endif

/**
 The provided block is executed when the receiver is rejected with the specified policy.

 @param policy The policy with which to catch. Either for all errors, or all errors *except* cancellation errors.

 @see catch
*/
- (AnyPromise * __nonnull(^ __nonnull)(PMKCatchPolicy, id __nonnull))catchWithPolicy;

/**
 The provided block is executed when the receiver is resolved.

 The provided block always runs on the main queue.

 @see finallyOn
*/
- (AnyPromise * __nonnull(^ __nonnull)(dispatch_block_t __nonnull))finally;

/**
 The provided block is executed on the dispatch queue of your choice when the receiver is resolved.

 @see finally
 */
- (AnyPromise * __nonnull(^ __nonnull)(dispatch_queue_t __nonnull, dispatch_block_t __nonnull))finallyOn;

/**
 The value of the asynchronous task this promise represents.

 A promise has `nil` value if the asynchronous task it represents has not
 finished. If the value is `nil` the promise is still `pending`.

 @warning *Note* Our Swift variant’s value property returns nil if the
 promise is rejected where AnyPromise will return the error object. This
 fits with the pattern where AnyPromise is not strictly typed and is more
 dynamic, but you should be aware of the distinction.

 @return If `resolved`, the object that was used to resolve this promise;
 if `pending`, nil.
*/
- (id __nullable)value;

/**
 Creates a resolved promise.

 When developing your own promise systems, it is ocassionally useful to be able to return an already resolved promise.

 @param value The value with which to resolve this promise. Passing an `NSError` will cause the promise to be rejected, otherwise the promise will be fulfilled.

 @return A resolved promise.
*/
+ (instancetype __nonnull)promiseWithValue:(id __nullable)value;

/**
 Create a new promise that resolves with the provided block.

 Use this method when wrapping asynchronous code that does *not* use
 promises so that this code can be used in promise chains.
 
 If `resolve` is called with an `NSError` object, the promise is
 rejected, otherwise the promise is fulfilled.

 Don’t use this method if you already have promises! Instead, just
 return your promise.

 Should you need to fulfill a promise but have no sensical value to use:
 your promise is a `void` promise: fulfill with `nil`.

 The block you pass is executed immediately on the calling thread.

 @param block The provided block is immediately executed, inside the block
 call `resolve` to resolve this promise and cause any attached handlers to
 execute. If you are wrapping a delegate-based system, we recommend
 instead to use: promiseWithResolver:

 @return A new promise.
 
 @warning *Important* Resolving a promise with `nil` fulfills it.

 @see http://promisekit.org/sealing-your-own-promises/
 @see http://promisekit.org/wrapping-delegation/
*/
+ (instancetype __nonnull)promiseWithResolverBlock:(void (^ __nonnull)(PMKResolver __nonnull resolve))resolverBlock;

/**
 Create a new promise with an associated resolver.

 Use this method when wrapping asynchronous code that does *not* use
 promises so that this code can be used in promise chains. Generally,
 prefer resolverWithBlock: as the resulting code is more elegant.

    PMKResolver resolve;
    AnyPromise *promise = [AnyPromise promiseWithResolver:&resolve];

    // later
    resolve(@"foo");

 @param resolver A reference to a block pointer of PMKResolver type.
 You can then call your resolver to resolve this promise.

 @return A new promise.

 @warning *Important* The resolver strongly retains the promise.

 @see promiseWithResolverBlock:
*/
- (instancetype __nonnull)initWithResolver:(PMKResolver __strong __nonnull * __nonnull)resolver;

@end



@interface AnyPromise (Unavailable)

- (instancetype __nonnull)init __attribute__((unavailable("It is illegal to create an unresolvable promise.")));
+ (instancetype __nonnull)new __attribute__((unavailable("It is illegal to create an unresolvable promise.")));

@end



typedef void (^PMKAdapter)(id __nullable, NSError * __nullable);
typedef void (^PMKIntegerAdapter)(NSInteger, NSError * __nullable);
typedef void (^PMKBooleanAdapter)(BOOL, NSError * __nullable);

@interface AnyPromise (Adapters)

/**
 Create a new promise by adapting an existing asynchronous system.

 The pattern of a completion block that passes two parameters, the first
 the result and the second an `NSError` object is so common that we
 provide this convenience adapter to make wrapping such systems more
 elegant.

    return [PMKPromise promiseWithAdapter:^(PMKAdapter adapter){
        PFQuery *query = [PFQuery …];
        [query findObjectsInBackgroundWithBlock:adapter];
    }];

 @warning *Important* If both parameters are nil, the promise fulfills,
 if both are non-nil the promise rejects. This is per the convention.

 @see http://promisekit.org/sealing-your-own-promises/
 */
+ (instancetype __nonnull)promiseWithAdapterBlock:(void (^ __nonnull)(PMKAdapter __nonnull adapter))block;

/**
 Create a new promise by adapting an existing asynchronous system.

 Adapts asynchronous systems that complete with `^(NSInteger, NSError *)`.
 NSInteger will cast to enums provided the enum has been wrapped with
 `NS_ENUM`. All of Apple’s enums are, so if you find one that hasn’t you
 may need to make a pull-request.

 @see promiseWithAdapter
 */
+ (instancetype __nonnull)promiseWithIntegerAdapterBlock:(void (^ __nonnull)(PMKIntegerAdapter __nonnull adapter))block;

/**
 Create a new promise by adapting an existing asynchronous system.

 Adapts asynchronous systems that complete with `^(BOOL, NSError *)`.

 @see promiseWithAdapter
 */
+ (instancetype __nonnull)promiseWithBooleanAdapterBlock:(void (^ __nonnull)(PMKBooleanAdapter __nonnull adapter))block;

@end



/**
 Whenever resolving a promise you may resolve with a tuple, eg.
 returning from a `then` or `catch` handler or resolving a new promise.

 Consumers of your Promise are not compelled to consume any arguments and
 in fact will often only consume the first parameter. Thus ensure the
 order of parameters is: from most-important to least-important.

 Currently PromiseKit limits you to THREE parameters to the manifold.
*/
#define PMKManifold(...) __PMKManifold(__VA_ARGS__, 3, 2, 1)
#define __PMKManifold(_1, _2, _3, N, ...) __PMKArrayWithCount(N, _1, _2, _3)
extern id __nonnull __PMKArrayWithCount(NSUInteger, ...);
