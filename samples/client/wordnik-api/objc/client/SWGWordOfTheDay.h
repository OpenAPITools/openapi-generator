#import <Foundation/Foundation.h>
#import "SWGObject.h"
#import "SWGDate.h"
#import "SWGSimpleExample.h"
#import "SWGSimpleDefinition.h"
#import "SWGContentProvider.h"


@interface SWGWordOfTheDay : SWGObject

@property(nonatomic) NSNumber* _id;  

@property(nonatomic) NSString* parentId;  

@property(nonatomic) NSString* category;  

@property(nonatomic) NSString* createdBy;  

@property(nonatomic) SWGDate* createdAt;  

@property(nonatomic) SWGContentProvider* contentProvider;  

@property(nonatomic) NSString* htmlExtra;  

@property(nonatomic) NSString* word;  

@property(nonatomic) NSArray* definitions;  

@property(nonatomic) NSArray* examples;  

@property(nonatomic) NSString* note;  

@property(nonatomic) SWGDate* publishDate;  

- (id) _id: (NSNumber*) _id
     parentId: (NSString*) parentId
     category: (NSString*) category
     createdBy: (NSString*) createdBy
     createdAt: (SWGDate*) createdAt
     contentProvider: (SWGContentProvider*) contentProvider
     htmlExtra: (NSString*) htmlExtra
     word: (NSString*) word
     definitions: (NSArray*) definitions
     examples: (NSArray*) examples
     note: (NSString*) note
     publishDate: (SWGDate*) publishDate;

- (id) initWithValues: (NSDictionary*)dict;
- (NSDictionary*) asDictionary;


@end

