#import <Foundation/NSError.h>

#if !defined(SWIFT_PASTE)
# define SWIFT_PASTE_HELPER(x, y) x##y
# define SWIFT_PASTE(x, y) SWIFT_PASTE_HELPER(x, y)
#endif

#if !defined(SWIFT_EXTENSION)
# define SWIFT_EXTENSION(M) SWIFT_PASTE(M##_Swift_, __LINE__)
#endif

@interface NSError (SWIFT_EXTENSION(PromiseKit))
+ (NSError * __nonnull)cancelledError;
+ (void)registerCancelledErrorDomain:(NSString * __nonnull)domain code:(NSInteger)code;
@property (nonatomic, readonly) BOOL cancelled;
@end
