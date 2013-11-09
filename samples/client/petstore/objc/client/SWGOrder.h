#import <Foundation/Foundation.h>
#import "SWGObject.h"
#import "SWGDate.h"

@interface SWGOrder : SWGObject

@property(nonatomic) NSNumber* _id;  /* Unique identifier for the order [optional]*/

@property(nonatomic) NSNumber* petId;  /* ID of pet being ordered [optional]*/

@property(nonatomic) NSNumber* quantity;  /* Number of pets ordered [optional]*/

@property(nonatomic) NSString* status;  /* Status of the order [optional]*/

@property(nonatomic) SWGDate* shipDate;  /* Date shipped, only if it has been [optional]*/

- (id) _id: (NSNumber*) _id
     petId: (NSNumber*) petId
     quantity: (NSNumber*) quantity
     status: (NSString*) status
     shipDate: (SWGDate*) shipDate;

- (id) initWithValues: (NSDictionary*)dict;
- (NSDictionary*) asDictionary;


@end

