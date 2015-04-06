#import <Foundation/Foundation.h>
#import "SWGObject.h"


@protocol SWGOrder
@end
  
@interface SWGOrder : SWGObject


@property(nonatomic) NSNumber<Optional>* _id;

@property(nonatomic) NSNumber<Optional>* petId;

@property(nonatomic) NSNumber<Optional>* quantity;

@property(nonatomic) NSDate<Optional>* shipDate;
/* Order Status [optional]
 */
@property(nonatomic) NSString<Optional>* status;

@property(nonatomic) NSNumber<Optional>* complete;

@end
