#import <Foundation/Foundation.h>
#import "SWGObject.h"


@interface SWGRelated : SWGObject

@property(nonatomic) NSString* label1;  

@property(nonatomic) NSString* relationshipType;  

@property(nonatomic) NSString* label2;  

@property(nonatomic) NSString* label3;  

@property(nonatomic) NSArray* words;  

@property(nonatomic) NSString* gram;  

@property(nonatomic) NSString* label4;  

- (id) label1: (NSString*) label1
     relationshipType: (NSString*) relationshipType
     label2: (NSString*) label2
     label3: (NSString*) label3
     words: (NSArray*) words
     gram: (NSString*) gram
     label4: (NSString*) label4;

- (id) initWithValues: (NSDictionary*)dict;
- (NSDictionary*) asDictionary;


@end

