#import "SWGDate.h"
#import "SWGCategory.h"

@implementation SWGCategory

-(id)_id: (NSNumber*) _id
    name: (NSString*) name
    
{
    __id = _id;
    _name = name;
    

    return self;
}

-(id) initWithValues:(NSDictionary*)dict
{
    self = [super init];
    if(self) {
        __id = dict[@"id"];
        
        _name = dict[@"name"];
        
        
    }
    return self;
}

-(NSDictionary*) asDictionary {
    NSMutableDictionary* dict = [[NSMutableDictionary alloc] init];
    
    
            if(__id != nil) dict[@"id"] = __id ;
        
    
    
            if(_name != nil) dict[@"name"] = _name ;
        
    

    NSDictionary* output = [dict copy];
    return output;
}

@end
