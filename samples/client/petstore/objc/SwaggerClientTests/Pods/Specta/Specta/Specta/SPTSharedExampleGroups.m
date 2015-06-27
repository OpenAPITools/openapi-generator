#import "SPTSharedExampleGroups.h"
#import "SPTExampleGroup.h"
#import "SPTSpec.h"
#import "SpectaUtility.h"
#import <objc/runtime.h>

NSMutableDictionary *globalSharedExampleGroups = nil;
BOOL initialized = NO;

@implementation SPTSharedExampleGroups

+ (void)initialize {
  Class SPTSharedExampleGroupsClass = [SPTSharedExampleGroups class];
  if ([self class] == SPTSharedExampleGroupsClass) {
    if (!initialized) {
      initialized = YES;
      globalSharedExampleGroups = [[NSMutableDictionary alloc] init];

      Class *classes = NULL;
      int numClasses = objc_getClassList(NULL, 0);

      if (numClasses > 0) {
        classes = (Class *)malloc(sizeof(Class) * numClasses);
        numClasses = objc_getClassList(classes, numClasses);

        Class klass, superClass;
        for(uint i = 0; i < numClasses; i++) {
          klass = classes[i];
          superClass = class_getSuperclass(klass);
          if (superClass == SPTSharedExampleGroupsClass) {
            [[[klass alloc] init] sharedExampleGroups];
          }
        }

        free(classes);
      }
    }
  }
}

+ (void)addSharedExampleGroupWithName:(NSString *)name block:(SPTDictionaryBlock)block exampleGroup:(SPTExampleGroup *)exampleGroup {
  (exampleGroup == nil ? globalSharedExampleGroups : exampleGroup.sharedExamples)[name] = [block copy];
}

+ (SPTDictionaryBlock)sharedExampleGroupWithName:(NSString *)name exampleGroup:(SPTExampleGroup *)exampleGroup {
  SPTDictionaryBlock sharedExampleGroup = nil;
  while (exampleGroup != nil) {
    if ((sharedExampleGroup = exampleGroup.sharedExamples[name])) {
      return sharedExampleGroup;
    }
    exampleGroup = exampleGroup.parent;
  }
  return globalSharedExampleGroups[name];
}

- (void)sharedExampleGroups {}

- (void)spt_handleException:(NSException *)exception {
  [SPTCurrentSpec spt_handleException:exception];
}

- (void)recordFailureWithDescription:(NSString *)description inFile:(NSString *)filename atLine:(NSUInteger)lineNumber expected:(BOOL)expected {
  [SPTCurrentSpec recordFailureWithDescription:description inFile:filename atLine:lineNumber expected:expected];
}

- (void)_recordUnexpectedFailureWithDescription:(NSString *)description exception:(NSException *)exception {
  [SPTCurrentSpec _recordUnexpectedFailureWithDescription:description exception:exception];
}

- (_XCTestCaseImplementation *)internalImplementation {
  return [SPTCurrentSpec internalImplementation];
}

@end
