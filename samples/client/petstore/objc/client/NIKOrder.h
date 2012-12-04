#import <Foundation/Foundation.h>
#import "NIKSwaggerObject.h"
#import "NIKDate.h"

@interface NIKOrder : NIKSwaggerObject

@property(nonatomic) NSNumber* _id;
@property(nonatomic) NSNumber* petId;
@property(nonatomic) NSString* status;
@property(nonatomic) NSNumber* quantity;
@property(nonatomic) NIKDate* shipDate;
- (id) _id: (NSNumber*) _id
     petId: (NSNumber*) petId
     status: (NSString*) status
     quantity: (NSNumber*) quantity
     shipDate: (NIKDate*) shipDate;

- (id) initWithValues: (NSDictionary*)dict;
- (NSDictionary*) asDictionary;


@end

