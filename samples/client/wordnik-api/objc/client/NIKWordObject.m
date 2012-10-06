#import "NIKWordObject.h"

@implementation NIKWordObject

@synthesize _id = __id;
@synthesize originalWord = _originalWord;
@synthesize word = _word;
@synthesize suggestions = _suggestions;
@synthesize canonicalForm = _canonicalForm;
@synthesize vulgar = _vulgar;
- (id) _id: (NSNumber*) _id
       originalWord: (NSString*) originalWord
       word: (NSString*) word
       suggestions: (NSArray*) suggestions
       canonicalForm: (NSString*) canonicalForm
       vulgar: (NSString*) vulgar
       {
          __id = _id;
          _originalWord = originalWord;
          _word = word;
          _suggestions = suggestions;
          _canonicalForm = canonicalForm;
          _vulgar = vulgar;
          return self;
       }

- (id) initWithValues: (NSDictionary*)dict
{
    __id = [dict objectForKey:@"id"];
    _originalWord = [dict objectForKey:@"originalWord"];
    _word = [dict objectForKey:@"word"];
    _suggestions = [dict objectForKey:@"suggestions"];
    _canonicalForm = [dict objectForKey:@"canonicalForm"];
    _vulgar = [dict objectForKey:@"vulgar"];
    return self;
}

-(NSDictionary*) asDictionary {
    NSMutableDictionary* dict = [[NSMutableDictionary alloc] init];
    if(__id != nil) [dict setObject:__id forKey:@"id"];
    if(_originalWord != nil) [dict setObject:_originalWord forKey:@"originalWord"];
    if(_word != nil) [dict setObject:_word forKey:@"word"];
    if(_suggestions != nil) [dict setObject:_suggestions forKey:@"suggestions"];
    if(_canonicalForm != nil) [dict setObject:_canonicalForm forKey:@"canonicalForm"];
    if(_vulgar != nil) [dict setObject:_vulgar forKey:@"vulgar"];
    NSDictionary* output = [dict copy];
    return output;
}

@end

