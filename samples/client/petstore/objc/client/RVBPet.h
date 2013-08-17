#import <Foundation/Foundation.h>
#import "NIKSwaggerObject.h"
#import "RVBCategory.h"
#import "RVBTag.h"

@interface RVBPet : NIKSwaggerObject

@property(nonatomic) NSString* name;
@property(nonatomic) NSNumber* _id;
@property(nonatomic) NSArray* tags;
@property(nonatomic) NSString* status;
@property(nonatomic) NSArray* photoUrls;
@property(nonatomic) RVBCategory* category;
- (id) name: (NSString*) name
     _id: (NSNumber*) _id
     tags: (NSArray*) tags
     status: (NSString*) status
     photoUrls: (NSArray*) photoUrls
     category: (RVBCategory*) category;

- (id) initWithValues: (NSDictionary*)dict;
- (NSDictionary*) asDictionary;


@end

