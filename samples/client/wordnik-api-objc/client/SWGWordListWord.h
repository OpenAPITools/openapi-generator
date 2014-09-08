#import <Foundation/Foundation.h>
#import "SWGObject.h"
#import "SWGDate.h"


@interface SWGWordListWord : SWGObject

@property(nonatomic) NSNumber* _id;  

@property(nonatomic) NSString* word;  

@property(nonatomic) NSString* username;  

@property(nonatomic) NSNumber* userId;  

@property(nonatomic) SWGDate* createdAt;  

@property(nonatomic) NSNumber* numberCommentsOnWord;  

@property(nonatomic) NSNumber* numberLists;  

- (id) _id: (NSNumber*) _id
     word: (NSString*) word
     username: (NSString*) username
     userId: (NSNumber*) userId
     createdAt: (SWGDate*) createdAt
     numberCommentsOnWord: (NSNumber*) numberCommentsOnWord
     numberLists: (NSNumber*) numberLists;

- (id) initWithValues: (NSDictionary*)dict;
- (NSDictionary*) asDictionary;


@end

