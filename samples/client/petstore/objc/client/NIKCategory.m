#import "NIKDate.h"
#import "NIKCategory.h"

@implementation NIKCategory

@synthesize raw = _raw;

@synthesize _id = __id;
@synthesize name = _name;
- (id) _id: (NSNumber*) _id
       name: (NSString*) name
       {
          __id = _id;
          _name = name;
          return self;
       }

- (id) initWithValues: (NSDictionary*)dict
{
    __id = [dict objectForKey:@"id"];
    _name = [dict objectForKey:@"name"];
    self.raw = [[NSDictionary alloc] initWithDictionary:dict];
    return self;
}

-(NSDictionary*) asDictionary {
    NSMutableDictionary* dict = [[NSMutableDictionary alloc] init];
    if(__id != nil) [dict setObject:__id forKey:@"id"];
    if(_name != nil) [dict setObject:_name forKey:@"name"];
    NSDictionary* output = [dict copy];
    return output;
}

-(NSDictionary*) asRaw {
    return _raw;
}
@end

