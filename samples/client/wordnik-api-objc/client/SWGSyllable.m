#import "SWGDate.h"
#import "SWGSyllable.h"

@implementation SWGSyllable

-(id)text: (NSString*) text
    seq: (NSNumber*) seq
    type: (NSString*) type
{
  _text = text;
  _seq = seq;
  _type = type;
  return self;
}

-(id) initWithValues:(NSDictionary*)dict
{
    self = [super init];
    if(self) {
        _text = dict[@"text"]; 
        _seq = dict[@"seq"]; 
        _type = dict[@"type"]; 
        

    }
    return self;
}

-(NSDictionary*) asDictionary {
    NSMutableDictionary* dict = [[NSMutableDictionary alloc] init];
    if(_text != nil) dict[@"text"] = _text ;
        if(_seq != nil) dict[@"seq"] = _seq ;
        if(_type != nil) dict[@"type"] = _type ;
        NSDictionary* output = [dict copy];
    return output;
}

@end

