#import <Foundation/Foundation.h>
#import <dispatch/dispatch.h>
#import "fwd.h"

typedef void (^PMKResolver)(id __nullable) NS_REFINED_FOR_SWIFT;

typedef NS_ENUM(NSInteger, PMKCatchPolicy) {
    PMKCatchPolicyAllErrors,
    PMKCatchPolicyAllErrorsExceptCancellation
} NS_SWIFT_NAME(CatchPolicy);


#if __has_include("PromiseKit-Swift.h")
    #pragma clang diagnostic push
    #pragma clang diagnostic ignored"-Wdocumentation"
    #import "PromiseKit-Swift.h"
    #pragma clang diagnostic pop
#else
    // this hack because `AnyPromise` is Swift, but we add
    // our own methods via the below category. This hack is
    // only required while building PromiseKit since, once
    // built, the generated -Swift header exists.

    __attribute__((objc_subclassing_restricted)) __attribute__((objc_runtime_name("AnyPromise")))
    @interface AnyPromise : NSObject
    @property (nonatomic, readonly) BOOL resolved;
    @property (nonatomic, readonly) BOOL pending;
    @property (nonatomic, readonly) __nullable id value;
    + (instancetype __nonnull)promiseWithResolverBlock:(void (^ __nonnull)(__nonnull PMKResolver))resolveBlock;
    + (instancetype __nonnull)promiseWithValue:(__nullable id)value;
    @end
#endif


@interface AnyPromise (obj)

@property (nonatomic, readonly) __nullable id value;

/**
 The provided block is executed when its receiver is resolved.

 If you provide a block that takes a parameter, the value of the receiver will be passed as that parameter.

    [NSURLSession GET:url].then(^(NSData *data){
        // do something with data
    });

 @return A new promise that is resolved with the value returned from the provided block. For example:

    [NSURLSession GET:url].then(^(NSData *data){
        return data.length;
    }).then(^(NSNumber *number){
        //…
    });

 @warning *Important* The block passed to `then` may take zero, one, two or three arguments, and return an object or return nothing. This flexibility is why the method signature for then is `id`, which means you will not get completion for the block parameter, and must type it yourself. It is safe to type any block syntax here, so to start with try just: `^{}`.

 @warning *Important* If an `NSError` or `NSString` is thrown inside your block, or you return an `NSError` object the next `Promise` will be rejected. See `catch` for documentation on error handling.

 @warning *Important* `then` is always executed on the main queue.

 @see thenOn
 @see thenInBackground
*/
- (AnyPromise * __nonnull (^ __nonnull)(id __nonnull))then NS_REFINED_FOR_SWIFT;


/**
 The provided block is executed on the default queue when the receiver is fulfilled.

 This method is provided as a convenience for `thenOn`.

 @see then
 @see thenOn
*/
- (AnyPromise * __nonnull(^ __nonnull)(id __nonnull))thenInBackground NS_REFINED_FOR_SWIFT;

/**
 The provided block is executed on the dispatch queue of your choice when the receiver is fulfilled.

 @see then
 @see thenInBackground
*/
- (AnyPromise * __nonnull(^ __nonnull)(dispatch_queue_t __nonnull, id __nonnull))thenOn NS_REFINED_FOR_SWIFT;

#ifndef __cplusplus
/**
 The provided block is executed when the receiver is rejected.

 Provide a block of form `^(NSError *){}` or simply `^{}`. The parameter has type `id` to give you the freedom to choose either.

 The provided block always runs on the main queue.
 
 @warning *Note* Cancellation errors are not caught.
 
 @warning *Note* Since catch is a c++ keyword, this method is not available in Objective-C++ files. Instead use catchWithPolicy.

 @see catchWithPolicy
*/
- (AnyPromise * __nonnull(^ __nonnull)(id __nonnull))catch NS_REFINED_FOR_SWIFT;
#endif

/**
 The provided block is executed when the receiver is rejected with the specified policy.

 Specify the policy with which to catch as the first parameter to your block. Either for all errors, or all errors *except* cancellation errors.

 @see catch
*/
- (AnyPromise * __nonnull(^ __nonnull)(PMKCatchPolicy, id __nonnull))catchWithPolicy NS_REFINED_FOR_SWIFT;

/**
 The provided block is executed when the receiver is resolved.

 The provided block always runs on the main queue.

 @see alwaysOn
*/
- (AnyPromise * __nonnull(^ __nonnull)(dispatch_block_t __nonnull))always NS_REFINED_FOR_SWIFT;

/**
 The provided block is executed on the dispatch queue of your choice when the receiver is resolved.

 @see always
 */
- (AnyPromise * __nonnull(^ __nonnull)(dispatch_queue_t __nonnull, dispatch_block_t __nonnull))alwaysOn NS_REFINED_FOR_SWIFT;

/// @see always
- (AnyPromise * __nonnull(^ __nonnull)(dispatch_block_t __nonnull))finally __attribute__((deprecated("Use always")));
/// @see alwaysOn
- (AnyPromise * __nonnull(^ __nonnull)(dispatch_block_t __nonnull, dispatch_block_t __nonnull))finallyOn __attribute__((deprecated("Use always")));

/**
 Create a new promise with an associated resolver.

 Use this method when wrapping asynchronous code that does *not* use
 promises so that this code can be used in promise chains. Generally,
 prefer `promiseWithResolverBlock:` as the resulting code is more elegant.

     PMKResolver resolve;
     AnyPromise *promise = [[AnyPromise alloc] initWithResolver:&resolve];

     // later
     resolve(@"foo");

 @param resolver A reference to a block pointer of PMKResolver type.
 You can then call your resolver to resolve this promise.

 @return A new promise.

 @warning *Important* The resolver strongly retains the promise.

 @see promiseWithResolverBlock:
*/
- (instancetype __nonnull)initWithResolver:(PMKResolver __strong __nonnull * __nonnull)resolver NS_REFINED_FOR_SWIFT;

@end



@interface AnyPromise (Unavailable)

- (instancetype __nonnull)init __attribute__((unavailable("It is illegal to create an unresolvable promise.")));
+ (instancetype __nonnull)new __attribute__((unavailable("It is illegal to create an unresolvable promise.")));

@end



typedef void (^PMKAdapter)(id __nullable, NSError * __nullable) NS_REFINED_FOR_SWIFT;
typedef void (^PMKIntegerAdapter)(NSInteger, NSError * __nullable) NS_REFINED_FOR_SWIFT;
typedef void (^PMKBooleanAdapter)(BOOL, NSError * __nullable) NS_REFINED_FOR_SWIFT;


@interface AnyPromise (Adapters)

/**
 Create a new promise by adapting an existing asynchronous system.

 The pattern of a completion block that passes two parameters, the first
 the result and the second an `NSError` object is so common that we
 provide this convenience adapter to make wrapping such systems more
 elegant.

    return [PMKPromise promiseWithAdapterBlock:^(PMKAdapter adapter){
        PFQuery *query = [PFQuery …];
        [query findObjectsInBackgroundWithBlock:adapter];
    }];

 @warning *Important* If both parameters are nil, the promise fulfills,
 if both are non-nil the promise rejects. This is per the convention.

 @see http://promisekit.org/sealing-your-own-promises/
 */
+ (instancetype __nonnull)promiseWithAdapterBlock:(void (^ __nonnull)(PMKAdapter __nonnull adapter))block NS_REFINED_FOR_SWIFT;

/**
 Create a new promise by adapting an existing asynchronous system.

 Adapts asynchronous systems that complete with `^(NSInteger, NSError *)`.
 NSInteger will cast to enums provided the enum has been wrapped with
 `NS_ENUM`. All of Apple’s enums are, so if you find one that hasn’t you
 may need to make a pull-request.

 @see promiseWithAdapter
 */
+ (instancetype __nonnull)promiseWithIntegerAdapterBlock:(void (^ __nonnull)(PMKIntegerAdapter __nonnull adapter))block NS_REFINED_FOR_SWIFT;

/**
 Create a new promise by adapting an existing asynchronous system.

 Adapts asynchronous systems that complete with `^(BOOL, NSError *)`.

 @see promiseWithAdapter
 */
+ (instancetype __nonnull)promiseWithBooleanAdapterBlock:(void (^ __nonnull)(PMKBooleanAdapter __nonnull adapter))block NS_REFINED_FOR_SWIFT;

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



@interface AnyPromise (Deprecations)

+ (instancetype __nonnull)new:(__nullable id)resolvers __attribute__((unavailable("See +promiseWithResolverBlock:")));
+ (instancetype __nonnull)when:(__nullable id)promises __attribute__((unavailable("See PMKWhen()")));
+ (instancetype __nonnull)join:(__nullable id)promises __attribute__((unavailable("See PMKJoin()")));

@end


__attribute__((unavailable("See AnyPromise")))
@interface PMKPromise
@end
