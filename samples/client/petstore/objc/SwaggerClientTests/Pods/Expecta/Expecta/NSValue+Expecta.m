#import "NSValue+Expecta.h"
#import <objc/runtime.h>
#import "Expecta.h"

EXPFixCategoriesBug(NSValue_Expecta);

@implementation NSValue (Expecta)

static char _EXP_typeKey;

- (const char *)_EXP_objCType {
  return [(NSString *)objc_getAssociatedObject(self, &_EXP_typeKey) cStringUsingEncoding:NSASCIIStringEncoding];
}

- (void)set_EXP_objCType:(const char *)_EXP_objCType {
  objc_setAssociatedObject(self, &_EXP_typeKey,
                           @(_EXP_objCType),
                           OBJC_ASSOCIATION_COPY_NONATOMIC);
}

@end
