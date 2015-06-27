#import "ExpectaSupport.h"
#import "NSValue+Expecta.h"
#import "NSObject+Expecta.h"
#import "EXPUnsupportedObject.h"
#import "EXPFloatTuple.h"
#import "EXPDoubleTuple.h"
#import "EXPDefines.h"
#import <objc/runtime.h>

@interface NSObject (ExpectaXCTestRecordFailure)

// suppress warning
- (void)recordFailureWithDescription:(NSString *)description inFile:(NSString *)filename atLine:(NSUInteger)lineNumber expected:(BOOL)expected;

@end

id _EXPObjectify(const char *type, ...) {
  va_list v;
  va_start(v, type);
  id obj = nil;
  if(strcmp(type, @encode(char)) == 0) {
    char actual = (char)va_arg(v, int);
    obj = @(actual);
  } else if(strcmp(type, @encode(_Bool)) == 0) {
    _Static_assert(sizeof(_Bool) <= sizeof(int), "Expected _Bool to be subject to vararg type promotion");
    _Bool actual = (_Bool)va_arg(v, int);
    obj = @(actual);
  } else if(strcmp(type, @encode(double)) == 0) {
    double actual = (double)va_arg(v, double);
    obj = @(actual);
  } else if(strcmp(type, @encode(float)) == 0) {
    float actual = (float)va_arg(v, double);
    obj = @(actual);
  } else if(strcmp(type, @encode(int)) == 0) {
    int actual = (int)va_arg(v, int);
    obj = @(actual);
  } else if(strcmp(type, @encode(long)) == 0) {
    long actual = (long)va_arg(v, long);
    obj = @(actual);
  } else if(strcmp(type, @encode(long long)) == 0) {
    long long actual = (long long)va_arg(v, long long);
    obj = @(actual);
  } else if(strcmp(type, @encode(short)) == 0) {
    short actual = (short)va_arg(v, int);
    obj = @(actual);
  } else if(strcmp(type, @encode(unsigned char)) == 0) {
    unsigned char actual = (unsigned char)va_arg(v, unsigned int);
    obj = @(actual);
  } else if(strcmp(type, @encode(unsigned int)) == 0) {
    unsigned int actual = (int)va_arg(v, unsigned int);
    obj = @(actual);
  } else if(strcmp(type, @encode(unsigned long)) == 0) {
    unsigned long actual = (unsigned long)va_arg(v, unsigned long);
    obj = @(actual);
  } else if(strcmp(type, @encode(unsigned long long)) == 0) {
    unsigned long long actual = (unsigned long long)va_arg(v, unsigned long long);
    obj = @(actual);
  } else if(strcmp(type, @encode(unsigned short)) == 0) {
    unsigned short actual = (unsigned short)va_arg(v, unsigned int);
    obj = @(actual);
  } else if(strstr(type, @encode(EXPBasicBlock)) != NULL) {
      // @encode(EXPBasicBlock) returns @? as of clang 4.1.
      // This condition must occur before the test for id/class type,
      // otherwise blocks will be treated as vanilla objects.
      id actual = va_arg(v, EXPBasicBlock);
      obj = [[actual copy] autorelease];
  } else if((strstr(type, @encode(id)) != NULL) || (strstr(type, @encode(Class)) != 0)) {
    id actual = va_arg(v, id);
    obj = actual;
  } else if(strcmp(type, @encode(__typeof__(nil))) == 0) {
    obj = nil;
  } else if(strstr(type, "ff}{") != NULL) { //TODO: of course this only works for a 2x2 e.g. CGRect
    obj = [[[EXPFloatTuple alloc] initWithFloatValues:(float *)va_arg(v, float[4]) size:4] autorelease];
  } else if(strstr(type, "=ff}") != NULL) {
    obj = [[[EXPFloatTuple alloc] initWithFloatValues:(float *)va_arg(v, float[2]) size:2] autorelease];
  } else if(strstr(type, "=ffff}") != NULL) {
    obj = [[[EXPFloatTuple alloc] initWithFloatValues:(float *)va_arg(v, float[4]) size:4] autorelease];
  } else if(strstr(type, "dd}{") != NULL) { //TODO: same here
    obj = [[[EXPDoubleTuple alloc] initWithDoubleValues:(double *)va_arg(v, double[4]) size:4] autorelease];
  } else if(strstr(type, "=dd}") != NULL) {
    obj = [[[EXPDoubleTuple alloc] initWithDoubleValues:(double *)va_arg(v, double[2]) size:2] autorelease];
  } else if(strstr(type, "=dddd}") != NULL) {
    obj = [[[EXPDoubleTuple alloc] initWithDoubleValues:(double *)va_arg(v, double[4]) size:4] autorelease];
  } else if(type[0] == '{') {
    EXPUnsupportedObject *actual = [[[EXPUnsupportedObject alloc] initWithType:@"struct"] autorelease];
    obj = actual;
  } else if(type[0] == '(') {
    EXPUnsupportedObject *actual = [[[EXPUnsupportedObject alloc] initWithType:@"union"] autorelease];
    obj = actual;
  } else {
    void *actual = va_arg(v, void *);
    obj = (actual == NULL ? nil :[NSValue valueWithPointer:actual]);
  }
  if([obj isKindOfClass:[NSValue class]] && ![obj isKindOfClass:[NSNumber class]]) {
    [(NSValue *)obj set_EXP_objCType:type];
  }
  va_end(v);
  return obj;
}

EXPExpect *_EXP_expect(id testCase, int lineNumber, const char *fileName, EXPIdBlock actualBlock) {
  return [EXPExpect expectWithActualBlock:actualBlock testCase:testCase lineNumber:lineNumber fileName:fileName];
}

void EXPFail(id testCase, int lineNumber, const char *fileName, NSString *message) {
  NSLog(@"%s:%d %@", fileName, lineNumber, message);
  NSString *reason = [NSString stringWithFormat:@"%s:%d %@", fileName, lineNumber, message];
  NSException *exception = [NSException exceptionWithName:@"Expecta Error" reason:reason userInfo:nil];

  if(testCase && [testCase respondsToSelector:@selector(recordFailureWithDescription:inFile:atLine:expected:)]){
      [testCase recordFailureWithDescription:message
                                      inFile:@(fileName)
                                      atLine:lineNumber
                                    expected:NO];
  } else {
    [exception raise];
  }
}

NSString *EXPDescribeObject(id obj) {
  if(obj == nil) {
    return @"nil/null";
  } else if([obj isKindOfClass:[NSValue class]] && ![obj isKindOfClass:[NSNumber class]]) {
    const char *type = [(NSValue *)obj _EXP_objCType];
    if(type) {
      if(strcmp(type, @encode(SEL)) == 0) {
        return [NSString stringWithFormat:@"@selector(%@)", NSStringFromSelector([obj pointerValue])];
      } else if(strcmp(type, @encode(Class)) == 0) {
        return NSStringFromClass([obj pointerValue]);
      }
    }
  }
  NSString *description = [obj description];
  if([obj isKindOfClass:[NSArray class]]) {
    NSMutableArray *arr = [NSMutableArray arrayWithCapacity:[obj count]];
    for(id o in obj) {
      [arr addObject:EXPDescribeObject(o)];
    }
    description = [NSString stringWithFormat:@"(%@)", [arr componentsJoinedByString:@", "]];
  } else if([obj isKindOfClass:[NSSet class]] || [obj isKindOfClass:[NSOrderedSet class]]) {
    NSMutableArray *arr = [NSMutableArray arrayWithCapacity:[obj count]];
    for(id o in obj) {
      [arr addObject:EXPDescribeObject(o)];
    }
    description = [NSString stringWithFormat:@"{(%@)}", [arr componentsJoinedByString:@", "]];
  } else if([obj isKindOfClass:[NSDictionary class]]) {
    NSMutableArray *arr = [NSMutableArray arrayWithCapacity:[obj count]];
    for(id k in obj) {
      id v = obj[k];
      [arr addObject:[NSString stringWithFormat:@"%@ = %@;",EXPDescribeObject(k), EXPDescribeObject(v)]];
    }
    description = [NSString stringWithFormat:@"{%@}", [arr componentsJoinedByString:@" "]];
  } else if([obj isKindOfClass:[NSAttributedString class]]) {
    description = [obj string];
  } else {
    description = [description stringByReplacingOccurrencesOfString:@"\n" withString:@"\\n"];
  }
  return description;
}

void EXP_prerequisite(EXPBoolBlock block) {
  [[[NSThread currentThread] threadDictionary][@"EXP_currentMatcher"] setPrerequisiteBlock:block];
}

void EXP_match(EXPBoolBlock block) {
  [[[NSThread currentThread] threadDictionary][@"EXP_currentMatcher"] setMatchBlock:block];
}

void EXP_failureMessageForTo(EXPStringBlock block) {
  [[[NSThread currentThread] threadDictionary][@"EXP_currentMatcher"] setFailureMessageForToBlock:block];
}

void EXP_failureMessageForNotTo(EXPStringBlock block) {
  [[[NSThread currentThread] threadDictionary][@"EXP_currentMatcher"] setFailureMessageForNotToBlock:block];
}

