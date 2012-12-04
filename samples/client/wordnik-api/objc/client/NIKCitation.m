#import "NIKDate.h"
#import "NIKCitation.h"

@implementation NIKCitation

@synthesize cite = _cite;
@synthesize source = _source;
- (id) cite: (NSString*) cite
       source: (NSString*) source
       {
          _cite = cite;
          _source = source;
          return self;
       }

- (id) initWithValues: (NSDictionary*)dict
{
    _cite = [dict objectForKey:@"cite"];
    _source = [dict objectForKey:@"source"];
    return self;
}

-(NSDictionary*) asDictionary {
    NSMutableDictionary* dict = [[NSMutableDictionary alloc] init];
    if(_cite != nil) [dict setObject:_cite forKey:@"cite"];
    if(_source != nil) [dict setObject:_source forKey:@"source"];
    NSDictionary* output = [dict copy];
    return output;
}

@end

