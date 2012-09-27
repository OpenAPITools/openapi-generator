#import <Foundation/Foundation.h>
#import "NIKSwaggerObject.h"

@interface NIKSimpleDefinition : NIKSwaggerObject {
@private
    NSString* _text; //NSString
    NSString* _source; //NSString
    NSString* _note; //NSString
    NSString* _partOfSpeech; //NSString
    }



@property(nonatomic) NSString* text;
@property(nonatomic) NSString* source;
@property(nonatomic) NSString* note;
@property(nonatomic) NSString* partOfSpeech;
- (id) text: (NSString*) text
     source: (NSString*) source
     note: (NSString*) note
     partOfSpeech: (NSString*) partOfSpeech;

- (id) initWithValues: (NSDictionary*)dict;
- (NSDictionary*) asDictionary;


@end

