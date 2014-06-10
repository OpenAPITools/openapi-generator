#import "SWGDate.h"
#import "SWGTextPron.h"

@implementation SWGTextPron

-(id)raw: (NSString*) raw
    seq: (NSNumber*) seq
    rawType: (NSString*) rawType
{
  _raw = raw;
  _seq = seq;
  _rawType = rawType;
  return self;
}

-(id) initWithValues:(NSDictionary*)dict
{
    self = [super init];
    if(self) {
        _raw = dict[@"raw"]; 
        _seq = dict[@"seq"]; 
        _rawType = dict[@"rawType"]; 
        

    }
    return self;
}

-(NSDictionary*) asDictionary {
    NSMutableDictionary* dict = [[NSMutableDictionary alloc] init];
    if(_raw != nil) dict[@"raw"] = _raw ;
        if(_seq != nil) dict[@"seq"] = _seq ;
        if(_rawType != nil) dict[@"rawType"] = _rawType ;
        NSDictionary* output = [dict copy];
    return output;
}

@end

