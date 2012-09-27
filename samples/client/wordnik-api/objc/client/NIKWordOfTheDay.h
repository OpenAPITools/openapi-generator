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
    NSString* _word; //NSString
    NSString* _htmlExtra; //NSString
    NSArray* _definitions; //SimpleDefinition
    NSArray* _examples; //SimpleExample
    NIKDate* _publishDate; //NIKDate
    NSString* _note; //NSString
    }



@property(nonatomic) NSNumber* _id;
@property(nonatomic) NSString* parentId;
@property(nonatomic) NSString* category;
@property(nonatomic) NSString* createdBy;
@property(nonatomic) NIKDate* createdAt;
@property(nonatomic) NIKContentProvider* contentProvider;
@property(nonatomic) NSString* word;
@property(nonatomic) NSString* htmlExtra;
@property(nonatomic) NSArray* definitions;
@property(nonatomic) NSArray* examples;
@property(nonatomic) NIKDate* publishDate;
@property(nonatomic) NSString* note;
- (id) _id: (NSNumber*) _id
     parentId: (NSString*) parentId
     category: (NSString*) category
     createdBy: (NSString*) createdBy
     createdAt: (NIKDate*) createdAt
     contentProvider: (NIKContentProvider*) contentProvider
     word: (NSString*) word
     htmlExtra: (NSString*) htmlExtra
     definitions: (NSArray*) definitions
     examples: (NSArray*) examples
     publishDate: (NIKDate*) publishDate
     note: (NSString*) note;

- (id) initWithValues: (NSDictionary*)dict;
- (NSDictionary*) asDictionary;


@end

