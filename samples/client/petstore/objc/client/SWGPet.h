#import <Foundation/Foundation.h>
#import "SWGObject.h"
#import "SWGCategory.h"
#import "SWGTag.h"


@interface SWGPet : SWGObject

@property(nonatomic) NSNumber* _id;  /* unique identifier for the pet */

@property(nonatomic) SWGCategory* category;  

@property(nonatomic) NSString* name;  

@property(nonatomic) NSArray* photoUrls;  

@property(nonatomic) NSArray* tags;  

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

