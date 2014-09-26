#import <Foundation/Foundation.h>
#import "SWGObject.h"
#import "SWGDate.h"
#import "SWGMap.h"


@interface SWGOrder : SWGObject

@property(nonatomic) NSNumber* _id;  
@property(nonatomic) NSNumber* petId;  
@property(nonatomic) NSNumber* quantity;  
@property(nonatomic) SWGDate* shipDate;  
@property(nonatomic) NSString* status;  
@property(nonatomic) NSNumber* complete;  
@property(nonatomic) SWGMap* keyValuePairs;  
- (id) _id: (NSNumber*) _id
  
       petId: (NSNumber*) petId
  
       quantity: (NSNumber*) quantity
  
       shipDate: (SWGDate*) shipDate
  
       status: (NSString*) status
  
       complete: (NSNumber*) complete
  
       keyValuePairs: (SWGMap*) keyValuePairs;
       

- (id) initWithValues: (NSDictionary*)dict;
- (NSDictionary*) asDictionary;

@end
