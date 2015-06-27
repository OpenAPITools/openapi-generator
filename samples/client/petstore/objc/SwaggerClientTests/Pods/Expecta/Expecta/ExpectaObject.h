#import <Foundation/Foundation.h>

#define EXPObjectify(value) _EXPObjectify(@encode(__typeof__((value))), (value))
#define EXP_expect(actual) _EXP_expect(self, __LINE__, __FILE__, ^id{ return EXPObjectify((actual)); })
#define EXPMatcherInterface(matcherName, matcherArguments) _EXPMatcherInterface(matcherName, matcherArguments)
#define EXPMatcherImplementationBegin(matcherName, matcherArguments) _EXPMatcherImplementationBegin(matcherName, matcherArguments)
#define EXPMatcherImplementationEnd _EXPMatcherImplementationEnd
#define EXPMatcherAliasImplementation(newMatcherName, oldMatcherName, matcherArguments) _EXPMatcherAliasImplementation(newMatcherName, oldMatcherName, matcherArguments)

#define EXP_failure(message) EXPFail(self, __LINE__, __FILE__, message)


@interface Expecta : NSObject

+ (NSTimeInterval)asynchronousTestTimeout;
+ (void)setAsynchronousTestTimeout:(NSTimeInterval)timeout;

@end
