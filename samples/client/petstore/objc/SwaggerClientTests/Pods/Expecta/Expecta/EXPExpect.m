#import "EXPExpect.h"
#import "NSObject+Expecta.h"
#import "Expecta.h"
#import "EXPUnsupportedObject.h"
#import "EXPMatcher.h"
#import "EXPBlockDefinedMatcher.h"
#import <libkern/OSAtomic.h>

@implementation EXPExpect

@dynamic
  actual,
  to,
  toNot,
  notTo,
  will,
  willNot,
  after;

@synthesize
  actualBlock=_actualBlock,
  testCase=_testCase,
  negative=_negative,
  asynchronous=_asynchronous,
  timeout=_timeout,
  lineNumber=_lineNumber,
  fileName=_fileName;

- (instancetype)initWithActualBlock:(id)actualBlock testCase:(id)testCase lineNumber:(int)lineNumber fileName:(const char *)fileName {
  self = [super init];
  if(self) {
    self.actualBlock = actualBlock;
    self.testCase = testCase;
    self.negative = NO;
    self.asynchronous = NO;
    self.timeout = [Expecta asynchronousTestTimeout];
    self.lineNumber = lineNumber;
    self.fileName = fileName;
  }
  return self;
}

- (void)dealloc
{
  _actualBlock = nil;
  [super dealloc];
}

+ (EXPExpect *)expectWithActualBlock:(id)actualBlock testCase:(id)testCase lineNumber:(int)lineNumber fileName:(const char *)fileName {
  return [[[EXPExpect alloc] initWithActualBlock:actualBlock testCase:(id)testCase lineNumber:lineNumber fileName:fileName] autorelease];
}

#pragma mark -

- (EXPExpect *)to {
  return self;
}

- (EXPExpect *)toNot {
  self.negative = !self.negative;
  return self;
}

- (EXPExpect *)notTo {
  return [self toNot];
}

- (EXPExpect *)will {
  self.asynchronous = YES;
  return self;
}

- (EXPExpect *)willNot {
  return self.will.toNot;
}

- (EXPExpect *(^)(NSTimeInterval))after
{
  EXPExpect * (^block)(NSTimeInterval) = [^EXPExpect *(NSTimeInterval timeout) {
    self.asynchronous = YES;
    self.timeout = timeout;
    return self;
  } copy];

  return [block autorelease];
}

#pragma mark -

- (id)actual {
  if(self.actualBlock) {
    return self.actualBlock();
  }
  return nil;
}

- (void)applyMatcher:(id<EXPMatcher>)matcher
{
  id actual = [self actual];
  [self applyMatcher:matcher to:&actual];
}

- (void)applyMatcher:(id<EXPMatcher>)matcher to:(NSObject **)actual {
  if([*actual isKindOfClass:[EXPUnsupportedObject class]]) {
    EXPFail(self.testCase, self.lineNumber, self.fileName,
            [NSString stringWithFormat:@"expecting a %@ is not supported", ((EXPUnsupportedObject *)*actual).type]);
  } else {
    BOOL failed = NO;
    if([matcher respondsToSelector:@selector(meetsPrerequesiteFor:)] &&
       ![matcher meetsPrerequesiteFor:*actual]) {
      failed = YES;
    } else {
      BOOL matchResult = NO;
      if(self.asynchronous) {
        NSTimeInterval timeOut = self.timeout;
        NSDate *expiryDate = [NSDate dateWithTimeIntervalSinceNow:timeOut];
        while(1) {
          matchResult = [matcher matches:*actual];
          failed = self.negative ? matchResult : !matchResult;
          if(!failed || ([(NSDate *)[NSDate date] compare:expiryDate] == NSOrderedDescending)) {
            break;
          }
          [[NSRunLoop currentRunLoop] runUntilDate:[NSDate dateWithTimeIntervalSinceNow:0.01]];
          OSMemoryBarrier();
          *actual = self.actual;
        }
      } else {
        matchResult = [matcher matches:*actual];
      }
      failed = self.negative ? matchResult : !matchResult;
    }
    if(failed) {
      NSString *message = nil;

      if(self.negative) {
        if ([matcher respondsToSelector:@selector(failureMessageForNotTo:)]) {
          message = [matcher failureMessageForNotTo:*actual];
        }
      } else {
        if ([matcher respondsToSelector:@selector(failureMessageForTo:)]) {
          message = [matcher failureMessageForTo:*actual];
        }
      }
      if (message == nil) {
        message = @"Match Failed.";
      }

      EXPFail(self.testCase, self.lineNumber, self.fileName, message);
    }
  }
  self.negative = NO;
}

#pragma mark - Dynamic predicate dispatch

- (NSMethodSignature *)methodSignatureForSelector:(SEL)aSelector
{
  if ([self.actual respondsToSelector:aSelector]) {
    return [self.actual methodSignatureForSelector:aSelector];
  }
  return [super methodSignatureForSelector:aSelector];
}

- (void)forwardInvocation:(NSInvocation *)anInvocation
{
  if ([self.actual respondsToSelector:anInvocation.selector]) {
    EXPDynamicPredicateMatcher *matcher = [[EXPDynamicPredicateMatcher alloc] initWithExpectation:self selector:anInvocation.selector];
    [anInvocation setSelector:@selector(dispatch)];
    [anInvocation invokeWithTarget:matcher];
    [matcher release];
  }
  else {
    [super forwardInvocation:anInvocation];
  }
}

@end

@implementation EXPDynamicPredicateMatcher

- (instancetype)initWithExpectation:(EXPExpect *)expectation selector:(SEL)selector
{
  if ((self = [super init])) {
    _expectation = expectation;
    _selector = selector;
  }
  return self;
}

- (BOOL)matches:(id)actual
{
  return (BOOL)[actual performSelector:_selector];
}

- (NSString *)failureMessageForTo:(id)actual
{
  return [NSString stringWithFormat:@"expected %@ to be true", NSStringFromSelector(_selector)];
}

- (NSString *)failureMessageForNotTo:(id)actual
{
  return [NSString stringWithFormat:@"expected %@ to be false", NSStringFromSelector(_selector)];
}

- (void (^)(void))dispatch
{
  __block id blockExpectation = _expectation;

  return [[^{
    [blockExpectation applyMatcher:self];
  } copy] autorelease];
}

@end
