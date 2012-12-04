#import <Foundation/Foundation.h>
#import "NIKSwaggerObject.h"
#import "NIKDate.h"

@interface NIKWordList : NIKSwaggerObject {
@private
    NIKDate* _updatedAt; //NIKDate
    NSNumber* __id; //NSNumber
    NSString* _username; //NSString
    NSString* _permalink; //NSString
    NIKDate* _lastActivityAt; //NIKDate
    NIKDate* _createdAt; //NIKDate
    NSString* _description; //NSString
    NSNumber* _userId; //NSNumber
    NSString* _name; //NSString
    NSNumber* _numberWordsInList; //NSNumber
    NSString* _type; //NSString
    }



@property(nonatomic) NIKDate* updatedAt;
@property(nonatomic) NSNumber* _id;
@property(nonatomic) NSString* username;
@property(nonatomic) NSString* permalink;
@property(nonatomic) NIKDate* lastActivityAt;
@property(nonatomic) NIKDate* createdAt;
@property(nonatomic) NSString* description;
@property(nonatomic) NSNumber* userId;
@property(nonatomic) NSString* name;
@property(nonatomic) NSNumber* numberWordsInList;
@property(nonatomic) NSString* type;
- (id) updatedAt: (NIKDate*) updatedAt
     _id: (NSNumber*) _id
     username: (NSString*) username
     permalink: (NSString*) permalink
     lastActivityAt: (NIKDate*) lastActivityAt
     createdAt: (NIKDate*) createdAt
     description: (NSString*) description
     userId: (NSNumber*) userId
     name: (NSString*) name
     numberWordsInList: (NSNumber*) numberWordsInList
     type: (NSString*) type;

- (id) initWithValues: (NSDictionary*)dict;
- (NSDictionary*) asDictionary;


@end

