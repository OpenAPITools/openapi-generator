#import <XCTest/XCTest.h>

#if __IPHONE_OS_VERSION_MAX_ALLOWED >= 90000 || __MAC_OS_X_VERSION_MAX_ALLOWED >= 101100

@interface XCTestObservationCenter (SPTTestSuspention)

- (void)_suspendObservationForBlock:(void (^)(void))block;

@end

#else

@interface XCTestObservationCenter : NSObject

+ (id)sharedObservationCenter;
- (void)_suspendObservationForBlock:(void (^)(void))block;

@end

#endif

@protocol XCTestObservation <NSObject>
@end

@interface _XCTestDriverTestObserver : NSObject <XCTestObservation>

- (void)stopObserving;
- (void)startObserving;

@end

@interface _XCTestCaseImplementation : NSObject
@end

@interface XCTestCase ()

- (_XCTestCaseImplementation *)internalImplementation;
- (void)_recordUnexpectedFailureWithDescription:(NSString *)description exception:(NSException *)exception;

@end
