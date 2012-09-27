#import <Foundation/Foundation.h>
#import "NIKSwaggerObject.h"

@interface NIKRelated : NIKSwaggerObject {
@private
    NSString* _label1; //NSString
    NSString* _label2; //NSString
    NSString* _relationshipType; //NSString
    NSString* _label3; //NSString
    NSArray* _words; //NSString
    NSString* _label4; //NSString
    NSString* _gram; //NSString
    }



@property(nonatomic) NSString* label1;
@property(nonatomic) NSString* label2;
@property(nonatomic) NSString* relationshipType;
@property(nonatomic) NSString* label3;
@property(nonatomic) NSArray* words;
@property(nonatomic) NSString* label4;
@property(nonatomic) NSString* gram;
- (id) label1: (NSString*) label1
     label2: (NSString*) label2
     relationshipType: (NSString*) relationshipType
     label3: (NSString*) label3
     words: (NSArray*) words
     label4: (NSString*) label4
     gram: (NSString*) gram;

- (id) initWithValues: (NSDictionary*)dict;
- (NSDictionary*) asDictionary;


@end

