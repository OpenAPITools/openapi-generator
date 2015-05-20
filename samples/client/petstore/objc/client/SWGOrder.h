#import <Foundation/Foundation.h>
#import "SWGObject.h"


@protocol SWGOrder
@end
  
@interface SWGOrder : SWGObject


@property(nonatomic) NSNumber* _id;

@property(nonatomic) NSNumber* petId;

@property(nonatomic) NSNumber* quantity;

@property(nonatomic) NSDate* shipDate;
/* Order Status [optional]
 */
@property(nonatomic) NSString* status;

@property(nonatomic) BOOL complete;

@end
