#import <Foundation/Foundation.h>
#import "NIKSwaggerObject.h"
#import "NIKDate.h"

@interface NIKWordList : NIKSwaggerObject {
@private
    NSNumber* __id; //NSNumber
    NIKDate* _updatedAt; //NIKDate
    NSString* _username; //NSString
    NSString* _permalink; //NSString
    NSString* _description; //NSString
    NIKDate* _createdAt; //NIKDate
    NIKDate* _lastActivityAt; //NIKDate
    NSString* _name; //NSString
    NSNumber* _userId; //NSNumber
    NSNumber* _numberWordsInList; //NSNumber
    NSString* _type; //NSString
    }



@property(nonatomic) NSNumber* _id;
@property(nonatomic) NIKDate* updatedAt;
@property(nonatomic) NSString* username;
@property(nonatomic) NSString* permalink;
@property(nonatomic) NSString* description;
@property(nonatomic) NIKDate* createdAt;
@property(nonatomic) NIKDate* lastActivityAt;
@property(nonatomic) NSString* name;
@property(nonatomic) NSNumber* userId;
@property(nonatomic) NSNumber* numberWordsInList;
@property(nonatomic) NSString* type;
- (id) _id: (NSNumber*) _id
     updatedAt: (NIKDate*) updatedAt
     username: (NSString*) username
     permalink: (NSString*) permalink
     description: (NSString*) description
     createdAt: (NIKDate*) createdAt
     lastActivityAt: (NIKDate*) lastActivityAt
     name: (NSString*) name
     userId: (NSNumber*) userId
     numberWordsInList: (NSNumber*) numberWordsInList
     type: (NSString*) type;

- (id) initWithValues: (NSDictionary*)dict;
- (NSDictionary*) asDictionary;


@end

