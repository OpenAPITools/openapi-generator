#import <Foundation/Foundation.h>
#import "SpectaTypes.h"

@class SPTCallSite;

@interface SPTExample : NSObject

@property (nonatomic, copy) NSString *name;
@property (nonatomic, retain) SPTCallSite *callSite;
@property (nonatomic, copy) SPTVoidBlock block;

@property (nonatomic) BOOL pending;
@property (nonatomic, getter=isFocused) BOOL focused;

- (id)initWithName:(NSString *)name callSite:(SPTCallSite *)callSite focused:(BOOL)focused block:(SPTVoidBlock)block;

@end
