#import <Foundation/Foundation.h>

@interface SWGObject : NSObject
- (id) initWithValues:(NSDictionary*)dict;
- (NSDictionary*) asDictionary;
@end
