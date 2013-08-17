#import <Foundation/Foundation.h>
#import "NIKSwaggerObject.h"
#import "NIKDate.h"

@interface RVBOrder : NIKSwaggerObject

@property(nonatomic) NSNumber* _id;
@property(nonatomic) NSString* status;
@property(nonatomic) NSNumber* petId;
@property(nonatomic) NSNumber* quantity;
@property(nonatomic) NIKDate* shipDate;
- (id) _id: (NSNumber*) _id
     status: (NSString*) status
     petId: (NSNumber*) petId
     quantity: (NSNumber*) quantity
     shipDate: (NIKDate*) shipDate;

- (id) initWithValues: (NSDictionary*)dict;
- (NSDictionary*) asDictionary;


@end

