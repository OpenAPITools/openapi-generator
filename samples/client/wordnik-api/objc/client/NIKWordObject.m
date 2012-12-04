#import "NIKDate.h"
#import "NIKWordObject.h"

@implementation NIKWordObject

@synthesize _id = __id;
@synthesize word = _word;
@synthesize originalWord = _originalWord;
@synthesize suggestions = _suggestions;
@synthesize canonicalForm = _canonicalForm;
@synthesize vulgar = _vulgar;
- (id) _id: (NSNumber*) _id
       word: (NSString*) word
       originalWord: (NSString*) originalWord
       suggestions: (NSArray*) suggestions
       canonicalForm: (NSString*) canonicalForm
       vulgar: (NSString*) vulgar
       {
          __id = _id;
          _word = word;
          _originalWord = originalWord;
          _suggestions = suggestions;
          _canonicalForm = canonicalForm;
          _vulgar = vulgar;
          return self;
       }

- (id) initWithValues: (NSDictionary*)dict
{
    __id = [dict objectForKey:@"id"];
    _word = [dict objectForKey:@"word"];
    _originalWord = [dict objectForKey:@"originalWord"];
    _suggestions = [dict objectForKey:@"suggestions"];
    _canonicalForm = [dict objectForKey:@"canonicalForm"];
    _vulgar = [dict objectForKey:@"vulgar"];
    return self;
}

-(NSDictionary*) asDictionary {
    NSMutableDictionary* dict = [[NSMutableDictionary alloc] init];
    if(__id != nil) [dict setObject:__id forKey:@"id"];
    if(_word != nil) [dict setObject:_word forKey:@"word"];
    if(_originalWord != nil) [dict setObject:_originalWord forKey:@"originalWord"];
    if(_suggestions != nil) [dict setObject:_suggestions forKey:@"suggestions"];
    if(_canonicalForm != nil) [dict setObject:_canonicalForm forKey:@"canonicalForm"];
    if(_vulgar != nil) [dict setObject:_vulgar forKey:@"vulgar"];
    NSDictionary* output = [dict copy];
    return output;
}

@end

