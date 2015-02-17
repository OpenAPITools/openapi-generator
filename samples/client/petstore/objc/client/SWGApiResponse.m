#import "SWGDate.h"
#import "SWGApiResponse.h"

@implementation SWGApiResponse

-(id)code: (NSNumber*) code
    type: (NSString*) type
    message: (NSString*) message
    
{
    _code = code;
    _type = type;
    _message = message;
    

    return self;
}

-(id) initWithValues:(NSDictionary*)dict
{
    self = [super init];
    if(self) {
        _code = dict[@"code"];
        
        _type = dict[@"type"];
        
        _message = dict[@"message"];
        
        
    }
    return self;
}

-(NSDictionary*) asDictionary {
    NSMutableDictionary* dict = [[NSMutableDictionary alloc] init];
    
    
            if(_code != nil) dict[@"code"] = _code ;
        
    
    
            if(_type != nil) dict[@"type"] = _type ;
        
    
    
            if(_message != nil) dict[@"message"] = _message ;
        
    

    NSDictionary* output = [dict copy];
    return output;
}

@end
