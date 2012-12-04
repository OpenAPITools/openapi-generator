#import <Foundation/Foundation.h>
#import "NIKSwaggerObject.h"

@interface NIKWordObject : NIKSwaggerObject {
@private
    NSNumber* __id; //NSNumber
    NSString* _word; //NSString
    NSString* _originalWord; //NSString
    NSArray* _suggestions; //NSString
    NSString* _canonicalForm; //NSString
    NSString* _vulgar; //NSString
    }



@property(nonatomic) NSNumber* _id;
@property(nonatomic) NSString* word;
@property(nonatomic) NSString* originalWord;
@property(nonatomic) NSArray* suggestions;
@property(nonatomic) NSString* canonicalForm;
@property(nonatomic) NSString* vulgar;
- (id) _id: (NSNumber*) _id
     word: (NSString*) word
     originalWord: (NSString*) originalWord
     suggestions: (NSArray*) suggestions
     canonicalForm: (NSString*) canonicalForm
     vulgar: (NSString*) vulgar;

- (id) initWithValues: (NSDictionary*)dict;
- (NSDictionary*) asDictionary;


@end

