#import <Foundation/Foundation.h>
#import "NIKSwaggerObject.h"
#import "NIKDate.h"

@interface NIKWordListWord : NIKSwaggerObject {
@private
    NSNumber* __id; //NSNumber
    NSString* _username; //NSString
    NIKDate* _createdAt; //NIKDate
    NSNumber* _numberCommentsOnWord; //NSNumber
    NSNumber* _userId; //NSNumber
    NSString* _word; //NSString
    NSNumber* _numberLists; //NSNumber
    }



@property(nonatomic) NSNumber* _id;
@property(nonatomic) NSString* username;
@property(nonatomic) NIKDate* createdAt;
@property(nonatomic) NSNumber* numberCommentsOnWord;
@property(nonatomic) NSNumber* userId;
@property(nonatomic) NSString* word;
@property(nonatomic) NSNumber* numberLists;
- (id) _id: (NSNumber*) _id
     username: (NSString*) username
     createdAt: (NIKDate*) createdAt
     numberCommentsOnWord: (NSNumber*) numberCommentsOnWord
     userId: (NSNumber*) userId
     word: (NSString*) word
     numberLists: (NSNumber*) numberLists;

- (id) initWithValues: (NSDictionary*)dict;
- (NSDictionary*) asDictionary;


@end

