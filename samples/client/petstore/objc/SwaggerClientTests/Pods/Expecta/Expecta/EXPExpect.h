#import <Foundation/Foundation.h>
#import "EXPMatcher.h"
#import "EXPDefines.h"

@interface EXPExpect : NSObject {
  EXPIdBlock _actualBlock;
  id _testCase;
  int _lineNumber;
  char *_fileName;
  BOOL _negative;
  BOOL _asynchronous;
  NSTimeInterval _timeout;
}

@property(nonatomic, copy) EXPIdBlock actualBlock;
@property(nonatomic, readonly) id actual;
@property(nonatomic, assign) id testCase;
@property(nonatomic) int lineNumber;
@property(nonatomic) const char *fileName;
@property(nonatomic) BOOL negative;
@property(nonatomic) BOOL asynchronous;
@property(nonatomic) NSTimeInterval timeout;

@property(nonatomic, readonly) EXPExpect *to;
@property(nonatomic, readonly) EXPExpect *toNot;
@property(nonatomic, readonly) EXPExpect *notTo;
@property(nonatomic, readonly) EXPExpect *will;
@property(nonatomic, readonly) EXPExpect *willNot;
@property(nonatomic, readonly) EXPExpect *(^after)(NSTimeInterval timeInterval);

- (instancetype)initWithActualBlock:(id)actualBlock testCase:(id)testCase lineNumber:(int)lineNumber fileName:(const char *)fileName NS_DESIGNATED_INITIALIZER;
+ (EXPExpect *)expectWithActualBlock:(id)actualBlock testCase:(id)testCase lineNumber:(int)lineNumber fileName:(const char *)fileName;

- (void)applyMatcher:(id<EXPMatcher>)matcher;
- (void)applyMatcher:(id<EXPMatcher>)matcher to:(NSObject **)actual;

@end

@interface EXPDynamicPredicateMatcher : NSObject <EXPMatcher> {
  EXPExpect *_expectation;
  SEL _selector;
}
- (instancetype)initWithExpectation:(EXPExpect *)expectation selector:(SEL)selector NS_DESIGNATED_INITIALIZER;
@property (nonatomic, readonly, copy) void (^dispatch)(void);
@end
