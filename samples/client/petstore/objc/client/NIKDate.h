#import <Foundation/Foundation.h>
#import "NIKSwaggerObject.h"

@interface NIKDate : NIKSwaggerObject {
@private
    NSDate *_date;
}
@property(nonatomic, readonly) NSDate* date;

- (id) initWithValues: (NSString*)input;
-(NSString*) toString;
@end