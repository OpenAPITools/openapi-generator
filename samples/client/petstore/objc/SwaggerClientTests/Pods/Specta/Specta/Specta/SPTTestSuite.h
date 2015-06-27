#import <Foundation/Foundation.h>

@class
  SPTExample
, SPTExampleGroup
;

@interface SPTTestSuite : NSObject

@property (nonatomic, strong) SPTExampleGroup *rootGroup;
@property (nonatomic, strong) NSMutableArray *groupStack;
@property (nonatomic, strong) NSArray *compiledExamples;
@property (nonatomic, copy) NSString *fileName;
@property (nonatomic) NSUInteger lineNumber;
@property (nonatomic, getter = isDisabled) BOOL disabled;
@property (nonatomic) BOOL hasFocusedExamples;

- (SPTExampleGroup *)currentGroup;
- (void)compile;

@end
