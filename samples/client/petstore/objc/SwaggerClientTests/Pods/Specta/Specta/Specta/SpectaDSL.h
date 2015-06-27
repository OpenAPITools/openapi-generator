#import <XCTest/XCTest.h>

#define SpecBegin(name) _SPTSpecBegin(name, __FILE__, __LINE__)
#define SpecEnd         _SPTSpecEnd

#define SharedExamplesBegin(name)      _SPTSharedExampleGroupsBegin(name)
#define SharedExamplesEnd              _SPTSharedExampleGroupsEnd
#define SharedExampleGroupsBegin(name) _SPTSharedExampleGroupsBegin(name)
#define SharedExampleGroupsEnd         _SPTSharedExampleGroupsEnd

typedef void (^DoneCallback)(void);

OBJC_EXTERN void describe(NSString *name, void (^block)());
OBJC_EXTERN void fdescribe(NSString *name, void (^block)());

OBJC_EXTERN void context(NSString *name, void (^block)());
OBJC_EXTERN void fcontext(NSString *name, void (^block)());

OBJC_EXTERN void it(NSString *name, void (^block)());
OBJC_EXTERN void fit(NSString *name, void (^block)());

OBJC_EXTERN void example(NSString *name, void (^block)());
OBJC_EXTERN void fexample(NSString *name, void (^block)());

OBJC_EXTERN void specify(NSString *name, void (^block)());
OBJC_EXTERN void fspecify(NSString *name, void (^block)());

#define   pending(...) spt_pending_(__VA_ARGS__, nil)
#define xdescribe(...) spt_pending_(__VA_ARGS__, nil)
#define  xcontext(...) spt_pending_(__VA_ARGS__, nil)
#define  xexample(...) spt_pending_(__VA_ARGS__, nil)
#define       xit(...) spt_pending_(__VA_ARGS__, nil)
#define  xspecify(...) spt_pending_(__VA_ARGS__, nil)

OBJC_EXTERN void beforeAll(void (^block)());
OBJC_EXTERN void afterAll(void (^block)());

OBJC_EXTERN void beforeEach(void (^block)());
OBJC_EXTERN void afterEach(void (^block)());

OBJC_EXTERN void before(void (^block)());
OBJC_EXTERN void after(void (^block)());

OBJC_EXTERN void sharedExamplesFor(NSString *name, void (^block)(NSDictionary *data));
OBJC_EXTERN void sharedExamples(NSString *name, void (^block)(NSDictionary *data));

#define itShouldBehaveLike(...) spt_itShouldBehaveLike_(@(__FILE__), __LINE__, __VA_ARGS__)
#define      itBehavesLike(...) spt_itShouldBehaveLike_(@(__FILE__), __LINE__, __VA_ARGS__)

OBJC_EXTERN void waitUntil(void (^block)(DoneCallback done));
/**
 * Runs the @c block and waits until the @c done block is called or the
 * @c timeout has passed.
 *
 * @param timeout timeout for this @c block only; does not affect the global
 *      timeout, as @c setAsyncSpecTimeout() does.
 * @param ^block  runs test code
 */
OBJC_EXTERN void waitUntilTimeout(NSTimeInterval timeout, void (^block)(DoneCallback done));

OBJC_EXTERN void setAsyncSpecTimeout(NSTimeInterval timeout);

// ----------------------------------------------------------------------------

#define _SPTSpecBegin(name, file, line) \
@interface name##Spec : SPTSpec \
@end \
@implementation name##Spec \
- (void)spec { \
  [[self class] spt_setCurrentTestSuiteFileName:(@(file)) lineNumber:(line)];

#define _SPTSpecEnd \
} \
@end

#define _SPTSharedExampleGroupsBegin(name) \
@interface name##SharedExampleGroups : SPTSharedExampleGroups \
@end \
@implementation name##SharedExampleGroups \
- (void)sharedExampleGroups {

#define _SPTSharedExampleGroupsEnd \
} \
@end

OBJC_EXTERN void spt_it_(NSString *name, NSString *fileName, NSUInteger lineNumber, void (^block)());
OBJC_EXTERN void spt_fit_(NSString *name, NSString *fileName, NSUInteger lineNumber, void (^block)());
OBJC_EXTERN void spt_pending_(NSString *name, ...);
OBJC_EXTERN void spt_itShouldBehaveLike_(NSString *fileName, NSUInteger lineNumber, NSString *name, id dictionaryOrBlock);
OBJC_EXTERN void spt_itShouldBehaveLike_block(NSString *fileName, NSUInteger lineNumber, NSString *name, NSDictionary *(^block)());
