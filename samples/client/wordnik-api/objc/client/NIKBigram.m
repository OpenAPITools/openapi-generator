#import "NIKDate.h"
#import "NIKBigram.h"

@implementation NIKBigram

@synthesize count = _count;
@synthesize gram2 = _gram2;
@synthesize gram1 = _gram1;
@synthesize wlmi = _wlmi;
@synthesize mi = _mi;
- (id) count: (NSNumber*) count
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

- (id) initWithValues: (NSDictionary*)dict
{
    _count = [dict objectForKey:@"count"];
    _gram2 = [dict objectForKey:@"gram2"];
    _gram1 = [dict objectForKey:@"gram1"];
    _wlmi = [dict objectForKey:@"wlmi"];
    _mi = [dict objectForKey:@"mi"];
    return self;
}

-(NSDictionary*) asDictionary {
    NSMutableDictionary* dict = [[NSMutableDictionary alloc] init];
    if(_count != nil) [dict setObject:_count forKey:@"count"];
    if(_gram2 != nil) [dict setObject:_gram2 forKey:@"gram2"];
    if(_gram1 != nil) [dict setObject:_gram1 forKey:@"gram1"];
    if(_wlmi != nil) [dict setObject:_wlmi forKey:@"wlmi"];
    if(_mi != nil) [dict setObject:_mi forKey:@"mi"];
    NSDictionary* output = [dict copy];
    return output;
}

@end

