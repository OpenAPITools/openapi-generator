#import "SWGDate.h"
#import "SWGSimpleDefinition.h"

@implementation SWGSimpleDefinition

-(id)text: (NSString*) text
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

-(id) initWithValues:(NSDictionary*)dict
{
    self = [super init];
    if(self) {
        _text = dict[@"text"];
        
        _source = dict[@"source"];
        
        _note = dict[@"note"];
        
        _partOfSpeech = dict[@"partOfSpeech"];
        
        
    }
    return self;
}

-(NSDictionary*) asDictionary {
    NSMutableDictionary* dict = [[NSMutableDictionary alloc] init];
    
    
            if(_text != nil) dict[@"text"] = _text ;
        
    
    
            if(_source != nil) dict[@"source"] = _source ;
        
    
    
            if(_note != nil) dict[@"note"] = _note ;
        
    
    
            if(_partOfSpeech != nil) dict[@"partOfSpeech"] = _partOfSpeech ;
        
    

    NSDictionary* output = [dict copy];
    return output;
}

@end
