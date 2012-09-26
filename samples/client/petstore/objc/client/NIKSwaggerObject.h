#import <Foundation/Foundation.h>

@interface NIKSwaggerObject : NSObject
- (id) initWithValues: (NSDictionary*)dict;
- (NSDictionary*) asDictionary;
@end
