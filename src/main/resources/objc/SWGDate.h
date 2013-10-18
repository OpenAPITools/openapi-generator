#import <Foundation/Foundation.h>
#import "SWGObject.h"

@interface SWGDate : SWGObject {
@private
    NSDate *_date;
}
@property(nonatomic, readonly) NSDate* date;

- (id) initWithValues: (NSString*)input;
-(NSString*) toString;
@end