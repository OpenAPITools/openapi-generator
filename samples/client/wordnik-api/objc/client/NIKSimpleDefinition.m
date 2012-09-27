#import "NIKDate.h"
#import "NIKSimpleDefinition.h"

@implementation NIKSimpleDefinition

@synthesize text = _text;
@synthesize source = _source;
@synthesize note = _note;
@synthesize partOfSpeech = _partOfSpeech;
- (id) text: (NSString*) text
       source: (NSString*) source
       note: (NSString*) note
       partOfSpeech: (NSString*) partOfSpeech
       {
          _text = text;
          _source = source;
          _note = note;
          _partOfSpeech = partOfSpeech;
          return self;
       }

- (id) initWithValues: (NSDictionary*)dict
{
    /* isContainer: , baseType: NSString, complexType:  */
    _text = [dict objectForKey:@"text"];
    /* isContainer: , baseType: NSString, complexType:  */
    _source = [dict objectForKey:@"source"];
    /* isContainer: , baseType: NSString, complexType:  */
    _note = [dict objectForKey:@"note"];
    /* isContainer: , baseType: NSString, complexType:  */
    _partOfSpeech = [dict objectForKey:@"partOfSpeech"];
    return self;
}

-(NSDictionary*) asDictionary {
    NSMutableDictionary* dict = [[NSMutableDictionary alloc] init];
    if(_text != nil) [dict setObject:_text forKey:@"text"];
    if(_source != nil) [dict setObject:_source forKey:@"source"];
    if(_note != nil) [dict setObject:_note forKey:@"note"];
    if(_partOfSpeech != nil) [dict setObject:_partOfSpeech forKey:@"partOfSpeech"];
    NSDictionary* output = [dict copy];
    return output;
}

@end

