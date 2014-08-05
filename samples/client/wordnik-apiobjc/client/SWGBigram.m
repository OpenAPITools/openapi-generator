#import "SWGDate.h"
#import "SWGBigram.h"

@implementation SWGBigram

-(id)count: (NSNumber*) count
    gram2: (NSString*) gram2
    gram1: (NSString*) gram1
    wlmi: (NSNumber*) wlmi
    mi: (NSNumber*) mi
{
  _count = count;
  _gram2 = gram2;
  _gram1 = gram1;
  _wlmi = wlmi;
  _mi = mi;
  return self;
}

-(id) initWithValues:(NSDictionary*)dict
{
    self = [super init];
    if(self) {
        _count = dict[@"count"]; 
        _gram2 = dict[@"gram2"]; 
        _gram1 = dict[@"gram1"]; 
        _wlmi = dict[@"wlmi"]; 
        _mi = dict[@"mi"]; 
        

    }
    return self;
}

-(NSDictionary*) asDictionary {
    NSMutableDictionary* dict = [[NSMutableDictionary alloc] init];
    if(_count != nil) dict[@"count"] = _count ;
        if(_gram2 != nil) dict[@"gram2"] = _gram2 ;
        if(_gram1 != nil) dict[@"gram1"] = _gram1 ;
        if(_wlmi != nil) dict[@"wlmi"] = _wlmi ;
        if(_mi != nil) dict[@"mi"] = _mi ;
        NSDictionary* output = [dict copy];
    return output;
}

@end

