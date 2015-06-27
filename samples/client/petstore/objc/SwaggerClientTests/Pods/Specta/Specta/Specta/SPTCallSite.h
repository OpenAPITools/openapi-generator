#import <Foundation/Foundation.h>

@interface SPTCallSite : NSObject

@property (nonatomic, copy, readonly) NSString *file;
@property (nonatomic, readonly) NSUInteger line;

+ (instancetype)callSiteWithFile:(NSString *)file line:(NSUInteger)line;

- (instancetype)initWithFile:(NSString *)file line:(NSUInteger)line;

@end
