#import <Foundation/Foundation.h>

extern NSString * const spt_kCurrentTestSuiteKey;
extern NSString * const spt_kCurrentSpecKey;

#define SPTCurrentTestSuite [[NSThread mainThread] threadDictionary][spt_kCurrentTestSuiteKey]
#define SPTCurrentSpec  [[NSThread mainThread] threadDictionary][spt_kCurrentSpecKey]
#define SPTCurrentGroup     [SPTCurrentTestSuite currentGroup]
#define SPTGroupStack       [SPTCurrentTestSuite groupStack]

#define SPTReturnUnlessBlockOrNil(block) if ((block) && !SPTIsBlock((block))) return;
#define SPTIsBlock(obj) [(obj) isKindOfClass:NSClassFromString(@"NSBlock")]

BOOL spt_isSpecClass(Class aClass);
NSString *spt_underscorize(NSString *string);
NSArray *spt_map(NSArray *array, id (^block)(id obj, NSUInteger idx));
NSArray *spt_shuffle(NSArray *array);
unsigned int spt_seed();
