#import "SWGDate.h"
#import "SWGApiTokenStatus.h"

@implementation SWGApiTokenStatus

-(id)valid: (NSNumber*) valid
    token: (NSString*) token
    resetsInMillis: (NSNumber*) resetsInMillis
    remainingCalls: (NSNumber*) remainingCalls
    expiresInMillis: (NSNumber*) expiresInMillis
    totalRequests: (NSNumber*) totalRequests
    
{
    _valid = valid;
    _token = token;
    _resetsInMillis = resetsInMillis;
    _remainingCalls = remainingCalls;
    _expiresInMillis = expiresInMillis;
    _totalRequests = totalRequests;
    

    return self;
}

-(id) initWithValues:(NSDictionary*)dict
{
    self = [super init];
    if(self) {
        _valid = dict[@"valid"];
        
        _token = dict[@"token"];
        
        _resetsInMillis = dict[@"resetsInMillis"];
        
        _remainingCalls = dict[@"remainingCalls"];
        
        _expiresInMillis = dict[@"expiresInMillis"];
        
        _totalRequests = dict[@"totalRequests"];
        
        
    }
    return self;
}

-(NSDictionary*) asDictionary {
    NSMutableDictionary* dict = [[NSMutableDictionary alloc] init];
    
    
            if(_valid != nil) dict[@"valid"] = _valid ;
        
    
    
            if(_token != nil) dict[@"token"] = _token ;
        
    
    
            if(_resetsInMillis != nil) dict[@"resetsInMillis"] = _resetsInMillis ;
        
    
    
            if(_remainingCalls != nil) dict[@"remainingCalls"] = _remainingCalls ;
        
    
    
            if(_expiresInMillis != nil) dict[@"expiresInMillis"] = _expiresInMillis ;
        
    
    
            if(_totalRequests != nil) dict[@"totalRequests"] = _totalRequests ;
        
    

    NSDictionary* output = [dict copy];
    return output;
}

@end
