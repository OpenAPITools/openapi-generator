#import <Foundation/Foundation.h>
#import "NIKSwaggerObject.h"
#import "NIKCategory.h"
#import "NIKTag.h"

@interface NIKPet : NIKSwaggerObject {
@private
    NSNumber* __id; //NSNumber
    NSArray* _tags; //Tag
    NIKCategory* _category; //Category
    NSString* _status; //NSString
    NSString* _name; //NSString
    NSArray* _photoUrls; //NSString
    }



@property(nonatomic) NSNumber* _id;
@property(nonatomic) NSArray* tags;
@property(nonatomic) NIKCategory* category;
@property(nonatomic) NSString* status;
@property(nonatomic) NSString* name;
@property(nonatomic) NSArray* photoUrls;
- (id) _id: (NSNumber*) _id
     tags: (NSArray*) tags
     category: (NIKCategory*) category
     status: (NSString*) status
     name: (NSString*) name
     photoUrls: (NSArray*) photoUrls;

- (id) initWithValues: (NSDictionary*)dict;
- (NSDictionary*) asDictionary;


@end

