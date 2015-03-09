#import "SWGDate.h"
#import "SWGAuthenticationToken.h"

@implementation SWGAuthenticationToken

-(id)token: (NSString*) token
    userId: (NSNumber*) userId
    userSignature: (NSString*) userSignature
    
{
    _token = token;
    _userId = userId;
    _userSignature = userSignature;
    

    return self;
}

-(id) initWithValues:(NSDictionary*)dict
{
    self = [super init];
    if(self) {
        _token = dict[@"token"];
        
        _userId = dict[@"userId"];
        
        _userSignature = dict[@"userSignature"];
        
        
    }
    return self;
}

-(NSDictionary*) asDictionary {
    NSMutableDictionary* dict = [[NSMutableDictionary alloc] init];
    
    
            if(_token != nil) dict[@"token"] = _token ;
        
    
    
            if(_userId != nil) dict[@"userId"] = _userId ;
        
    
    
            if(_userSignature != nil) dict[@"userSignature"] = _userSignature ;
        
    

    NSDictionary* output = [dict copy];
    return output;
}

@end
