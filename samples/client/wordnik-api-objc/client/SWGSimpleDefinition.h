#import <Foundation/Foundation.h>
#import "SWGObject.h"


@interface SWGSimpleDefinition : SWGObject

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

