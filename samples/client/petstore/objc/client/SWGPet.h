#import <Foundation/Foundation.h>
#import "SWGObject.h"
#import "SWGTag.h"
#import "SWGCategory.h"


@protocol SWGPet
@end
  
@interface SWGPet : SWGObject


@property(nonatomic) NSNumber<Optional>* _id;

@property(nonatomic) SWGCategory<Optional, SWGCategory>* category;

@property(nonatomic) NSString* name;

@property(nonatomic) NSArray* photoUrls;

@property(nonatomic) NSArray<Optional, SWGTag>* tags;
/* pet status in the store [optional]
 */
@property(nonatomic) NSString<Optional>* status;

@end
