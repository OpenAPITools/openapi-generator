#import <Foundation/Foundation.h>
#import "SWGObject.h"
#import "SWGDate.h"


@interface SWGOrder : SWGObject

@property(nonatomic) NSNumber* _id;  

@property(nonatomic) NSNumber* petId;  

@property(nonatomic) NSNumber* quantity;  

@property(nonatomic) NSString* status;  /* Order Status [optional]*/

@property(nonatomic) SWGDate* shipDate;  

- (id) _id: (NSNumber*) _id
     petId: (NSNumber*) petId
     quantity: (NSNumber*) quantity
     status: (NSString*) status
     shipDate: (SWGDate*) shipDate;

- (id) initWithValues: (NSDictionary*)dict;
- (NSDictionary*) asDictionary;


@end

