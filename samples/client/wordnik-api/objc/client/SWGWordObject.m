#import "SWGDate.h"
#import "SWGWordObject.h"

@implementation SWGWordObject

-(id)_id: (NSNumber*) _id
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

-(id) initWithValues:(NSDictionary*)dict
{
    self = [super init];
    if(self) {
        __id = dict[@"id"]; 
        _word = dict[@"word"]; 
        _originalWord = dict[@"originalWord"]; 
        _suggestions = dict[@"suggestions"]; 
        _canonicalForm = dict[@"canonicalForm"]; 
        _vulgar = dict[@"vulgar"]; 
        

    }
    return self;
}

-(NSDictionary*) asDictionary {
    NSMutableDictionary* dict = [[NSMutableDictionary alloc] init];
    if(__id != nil) dict[@"id"] = __id ;
        if(_word != nil) dict[@"word"] = _word ;
        if(_originalWord != nil) dict[@"originalWord"] = _originalWord ;
        if(_suggestions != nil) dict[@"suggestions"] = _suggestions ;
        if(_canonicalForm != nil) dict[@"canonicalForm"] = _canonicalForm ;
        if(_vulgar != nil) dict[@"vulgar"] = _vulgar ;
        NSDictionary* output = [dict copy];
    return output;
}

@end

