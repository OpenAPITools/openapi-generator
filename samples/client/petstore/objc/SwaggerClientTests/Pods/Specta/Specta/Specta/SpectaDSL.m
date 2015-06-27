#import "SpectaDSL.h"
#import "SpectaTypes.h"
#import "SpectaUtility.h"
#import "SPTTestSuite.h"
#import "SPTExampleGroup.h"
#import "SPTSharedExampleGroups.h"
#import "SPTSpec.h"
#import "SPTCallSite.h"
#import <libkern/OSAtomic.h>

static NSTimeInterval asyncSpecTimeout = 10.0;

static void spt_defineItBlock(NSString *name, NSString *fileName, NSUInteger lineNumber, BOOL focused, void (^block)()) {
  SPTReturnUnlessBlockOrNil(block);
  SPTCallSite *site = nil;
  if (lineNumber && fileName) {
    site = [SPTCallSite callSiteWithFile:fileName line:lineNumber];
  }
  [SPTCurrentGroup addExampleWithName:name callSite:site focused:focused block:block];
}

static void spt_defineDescribeBlock(NSString *name, BOOL focused, void (^block)()) {
  if (block) {
    [SPTGroupStack addObject:[SPTCurrentGroup addExampleGroupWithName:name focused:focused]];
    block();
    [SPTGroupStack removeLastObject];
  } else {
    spt_defineItBlock(name, nil, 0, focused, nil);
  }
}

void spt_it_(NSString *name, NSString *fileName, NSUInteger lineNumber, void (^block)()) {
  spt_defineItBlock(name, fileName, lineNumber, NO, block);
}

void spt_fit_(NSString *name, NSString *fileName, NSUInteger lineNumber, void (^block)()) {
  spt_defineItBlock(name, fileName, lineNumber, YES, block);
}

void spt_pending_(NSString *name, ...) {
  spt_defineItBlock(name, nil, 0, NO, nil);
}

void spt_itShouldBehaveLike_(NSString *fileName, NSUInteger lineNumber, NSString *name, id dictionaryOrBlock) {
  SPTDictionaryBlock block = [SPTSharedExampleGroups sharedExampleGroupWithName:name exampleGroup:SPTCurrentGroup];
  if (block) {
    if (SPTIsBlock(dictionaryOrBlock)) {
      id (^dataBlock)(void) = [dictionaryOrBlock copy];

      describe(name, ^{
        __block NSMutableDictionary *dataDict = [[NSMutableDictionary alloc] init];

        beforeEach(^{
          NSDictionary *blockData = dataBlock();
          [dataDict removeAllObjects];
          [dataDict addEntriesFromDictionary:blockData];
        });

        block(dataDict);

        afterAll(^{
          dataDict = nil;
        });
      });
    } else {
      NSDictionary *data = dictionaryOrBlock;

      describe(name, ^{
        block(data);
      });
    }
  } else {
    SPTSpec *currentSpec = SPTCurrentSpec;
    if (currentSpec) {
      [currentSpec recordFailureWithDescription:@"itShouldBehaveLike should not be invoked inside an example block!" inFile:fileName atLine:lineNumber expected:NO];
    } else {
      it(name, ^{
        [SPTCurrentSpec recordFailureWithDescription:[NSString stringWithFormat:@"Shared example group \"%@\" does not exist.", name] inFile:fileName atLine:lineNumber expected:NO];
      });
    }
  }
}

void spt_itShouldBehaveLike_block(NSString *fileName, NSUInteger lineNumber, NSString *name, NSDictionary *(^block)()) {
  spt_itShouldBehaveLike_(fileName, lineNumber, name, (id)block);
}

void describe(NSString *name, void (^block)()) {
  spt_defineDescribeBlock(name, NO, block);
}

void fdescribe(NSString *name, void (^block)()) {
  spt_defineDescribeBlock(name, YES, block);
}

void context(NSString *name, void (^block)()) {
  describe(name, block);
}

void fcontext(NSString *name, void (^block)()) {
  fdescribe(name, block);
}

void it(NSString *name, void (^block)()) {
  spt_defineItBlock(name, nil, 0, NO, block);
}

void fit(NSString *name, void (^block)()) {
  spt_defineItBlock(name, nil, 0, YES, block);
}

void example(NSString *name, void (^block)()) {
  it(name, block);
}

void fexample(NSString *name, void (^block)()) {
  fit(name, block);
}

void specify(NSString *name, void (^block)()) {
  it(name, block);
}

void fspecify(NSString *name, void (^block)()) {
  fit(name, block);
}

void beforeAll(void (^block)()) {
  SPTReturnUnlessBlockOrNil(block);
  [SPTCurrentGroup addBeforeAllBlock:block];
}

void afterAll(void (^block)()) {
  SPTReturnUnlessBlockOrNil(block);
  [SPTCurrentGroup addAfterAllBlock:block];
}

void beforeEach(void (^block)()) {
  SPTReturnUnlessBlockOrNil(block);
  [SPTCurrentGroup addBeforeEachBlock:block];
}

void afterEach(void (^block)()) {
  SPTReturnUnlessBlockOrNil(block);
  [SPTCurrentGroup addAfterEachBlock:block];
}

void before(void (^block)()) {
  beforeEach(block);
}

void after(void (^block)()) {
  afterEach(block);
}

void sharedExamplesFor(NSString *name, void (^block)(NSDictionary *data)) {
  [SPTSharedExampleGroups addSharedExampleGroupWithName:name block:block exampleGroup:SPTCurrentGroup];
}

void sharedExamples(NSString *name, void (^block)(NSDictionary *data)) {
  sharedExamplesFor(name, block);
}

void waitUntil(void (^block)(DoneCallback done)) {
  waitUntilTimeout(asyncSpecTimeout, block);
}

void waitUntilTimeout(NSTimeInterval timeout, void (^block)(DoneCallback done)) {
  __block uint32_t complete = 0;
  dispatch_async(dispatch_get_main_queue(), ^{
    block(^{
      OSAtomicOr32Barrier(1, &complete);
    });
  });
  NSDate *timeoutDate = [NSDate dateWithTimeIntervalSinceNow:timeout];
  while (!complete && [timeoutDate timeIntervalSinceNow] > 0) {
    [[NSRunLoop currentRunLoop] runUntilDate:[NSDate dateWithTimeIntervalSinceNow:0.01]];
  }
  if (!complete) {
    NSString *message = [NSString stringWithFormat:@"failed to invoke done() callback before timeout (%f seconds)", timeout];
    SPTSpec *currentSpec = SPTCurrentSpec;
    SPTTestSuite *testSuite = [[currentSpec class] spt_testSuite];
    [currentSpec recordFailureWithDescription:message inFile:testSuite.fileName atLine:testSuite.lineNumber expected:YES];
  }
}

void setAsyncSpecTimeout(NSTimeInterval timeout) {
  asyncSpecTimeout = timeout;
}
