#import "NIKDate.h"
#import "RVBTag.h"

@implementation RVBTag

-(id)name: (NSString*) name
    _id: (NSNumber*) _id
{
  _name = name;
  __id = _id;
  return self;
}

-(id) initWithValues:(NSDictionary*)dict
{
    self = [super init];
    if(self) {
        _name = dict[@"name"]; 
        __id = dict[@"id"]; 
        

    }
    return self;
}

-(NSDictionary*) asDictionary {
    NSMutableDictionary* dict = [[NSMutableDictionary alloc] init];
    if(_name != nil) dict[@"name"] = _name ;
    if(__id != nil) dict[@"id"] = __id ;
    NSDictionary* output = [dict copy];
    return output;
}

@end

