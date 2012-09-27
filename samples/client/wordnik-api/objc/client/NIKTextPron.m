#import "NIKDate.h"
#import "NIKTextPron.h"

@implementation NIKTextPron

@synthesize raw = _raw;
@synthesize seq = _seq;
@synthesize rawType = _rawType;
- (id) raw: (NSString*) raw
       seq: (NSNumber*) seq
       rawType: (NSString*) rawType
       {
          _raw = raw;
          _seq = seq;
          _rawType = rawType;
          return self;
       }

- (id) initWithValues: (NSDictionary*)dict
{
    _raw = [dict objectForKey:@"raw"];
    _seq = [dict objectForKey:@"seq"];
    _rawType = [dict objectForKey:@"rawType"];
    return self;
}

-(NSDictionary*) asDictionary {
    NSMutableDictionary* dict = [[NSMutableDictionary alloc] init];
    if(_raw != nil) [dict setObject:_raw forKey:@"raw"];
    if(_seq != nil) [dict setObject:_seq forKey:@"seq"];
    if(_rawType != nil) [dict setObject:_rawType forKey:@"rawType"];
    NSDictionary* output = [dict copy];
    return output;
}

@end

