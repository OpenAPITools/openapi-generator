#import <Foundation/Foundation.h>
#import "NIKSwaggerObject.h"
#import "NIKDate.h"
#import "NIKSimpleDefinition.h"
#import "NIKSimpleExample.h"
#import "NIKContentProvider.h"

@interface NIKWordOfTheDay : NIKSwaggerObject {
@private
    NSNumber* __id; //NSNumber
    NSString* _parentId; //NSString
    NSString* _category; //NSString
    NSString* _createdBy; //NSString
    NIKDate* _createdAt; //NIKDate
    NIKContentProvider* _contentProvider; //ContentProvider
    NSString* _htmlExtra; //NSString
    NSString* _word; //NSString
    NSArray* _definitions; //SimpleDefinition
    NSArray* _examples; //SimpleExample
    NSString* _note; //NSString
    NIKDate* _publishDate; //NIKDate
    }



@property(nonatomic) NSNumber* _id;
@property(nonatomic) NSString* parentId;
@property(nonatomic) NSString* category;
@property(nonatomic) NSString* createdBy;
@property(nonatomic) NIKDate* createdAt;
@property(nonatomic) NIKContentProvider* contentProvider;
@property(nonatomic) NSString* htmlExtra;
@property(nonatomic) NSString* word;
@property(nonatomic) NSArray* definitions;
@property(nonatomic) NSArray* examples;
@property(nonatomic) NSString* note;
@property(nonatomic) NIKDate* publishDate;
- (id) _id: (NSNumber*) _id
     parentId: (NSString*) parentId
     category: (NSString*) category
     createdBy: (NSString*) createdBy
     createdAt: (NIKDate*) createdAt
     contentProvider: (NIKContentProvider*) contentProvider
     htmlExtra: (NSString*) htmlExtra
     word: (NSString*) word
     definitions: (NSArray*) definitions
     examples: (NSArray*) examples
     note: (NSString*) note
     publishDate: (NIKDate*) publishDate;

- (id) initWithValues: (NSDictionary*)dict;
- (NSDictionary*) asDictionary;


@end

