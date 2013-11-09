#import <Foundation/Foundation.h>
#import "SWGObject.h"
#import "SWGCategory.h"
#import "SWGTag.h"

@interface SWGPet : SWGObject

@property(nonatomic) NSNumber* _id;  /* Unique identifier for the Pet */

@property(nonatomic) SWGCategory* category;  /* Category the pet is in [optional]*/

@property(nonatomic) NSString* name;  /* Friendly name of the pet */

@property(nonatomic) NSArray* photoUrls;  /* Image URLs [optional]*/

@property(nonatomic) NSArray* tags;  /* Tags assigned to this pet [optional]*/

@property(nonatomic) NSString* status;  /* pet status in the store [optional]*/

- (id) _id: (NSNumber*) _id
     category: (SWGCategory*) category
     name: (NSString*) name
     photoUrls: (NSArray*) photoUrls
     tags: (NSArray*) tags
     status: (NSString*) status;

- (id) initWithValues: (NSDictionary*)dict;
- (NSDictionary*) asDictionary;


@end

