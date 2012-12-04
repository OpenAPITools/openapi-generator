#import <Foundation/Foundation.h>
#import "NIKSwaggerObject.h"
#import "NIKCategory.h"
#import "NIKTag.h"

@interface NIKPet : NIKSwaggerObject

@property(nonatomic) NSArray* tags;
@property(nonatomic) NSNumber* _id;
@property(nonatomic) NIKCategory* category;
@property(nonatomic) NSString* status;
@property(nonatomic) NSString* name;
@property(nonatomic) NSArray* photoUrls;
- (id) tags: (NSArray*) tags
     _id: (NSNumber*) _id
     category: (NIKCategory*) category
     status: (NSString*) status
     name: (NSString*) name
     photoUrls: (NSArray*) photoUrls;

- (id) initWithValues: (NSDictionary*)dict;
- (NSDictionary*) asDictionary;


@end

