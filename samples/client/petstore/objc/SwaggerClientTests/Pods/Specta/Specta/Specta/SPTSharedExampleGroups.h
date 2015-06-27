#import <XCTest/XCTest.h>
#import <Specta/XCTestCase+Specta.h>
#import <Specta/SpectaTypes.h>

@class _XCTestCaseImplementation;

@class SPTExampleGroup;

@interface SPTSharedExampleGroups : XCTestCase

+ (void)addSharedExampleGroupWithName:(NSString *)name block:(SPTDictionaryBlock)block exampleGroup:(SPTExampleGroup *)exampleGroup;
+ (SPTDictionaryBlock)sharedExampleGroupWithName:(NSString *)name exampleGroup:(SPTExampleGroup *)exampleGroup;

- (void)sharedExampleGroups;

@end

