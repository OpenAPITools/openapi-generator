#import <Foundation/Foundation.h>
#import "SWGObject.h"
#import "SWGDate.h"


@interface SWGWordList : SWGObject

@property(nonatomic) NSNumber* _id;  

@property(nonatomic) NSString* permalink;  

@property(nonatomic) NSString* name;  

@property(nonatomic) SWGDate* createdAt;  

@property(nonatomic) SWGDate* updatedAt;  

@property(nonatomic) SWGDate* lastActivityAt;  

@property(nonatomic) NSString* username;  

@property(nonatomic) NSNumber* userId;  

@property(nonatomic) NSString* description;  

@property(nonatomic) NSNumber* numberWordsInList;  

@property(nonatomic) NSString* type;  

- (id) _id: (NSNumber*) _id
     permalink: (NSString*) permalink
     name: (NSString*) name
     createdAt: (SWGDate*) createdAt
     updatedAt: (SWGDate*) updatedAt
     lastActivityAt: (SWGDate*) lastActivityAt
     username: (NSString*) username
     userId: (NSNumber*) userId
     description: (NSString*) description
     numberWordsInList: (NSNumber*) numberWordsInList
     type: (NSString*) type;

- (id) initWithValues: (NSDictionary*)dict;
- (NSDictionary*) asDictionary;


@end

