#import "NIKDate.h"
#import "NIKApiTokenStatus.h"

@implementation NIKApiTokenStatus

@synthesize valid = _valid;
@synthesize token = _token;
@synthesize resetsInMillis = _resetsInMillis;
@synthesize remainingCalls = _remainingCalls;
@synthesize expiresInMillis = _expiresInMillis;
@synthesize totalRequests = _totalRequests;
- (id) valid: (NSNumber*) valid
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

- (id) initWithValues: (NSDictionary*)dict
{
    _valid = [dict objectForKey:@"valid"];
    _token = [dict objectForKey:@"token"];
    _resetsInMillis = [dict objectForKey:@"resetsInMillis"];
    _remainingCalls = [dict objectForKey:@"remainingCalls"];
    _expiresInMillis = [dict objectForKey:@"expiresInMillis"];
    _totalRequests = [dict objectForKey:@"totalRequests"];
    return self;
}

-(NSDictionary*) asDictionary {
    NSMutableDictionary* dict = [[NSMutableDictionary alloc] init];
    if(_valid != nil) [dict setObject:_valid forKey:@"valid"];
    if(_token != nil) [dict setObject:_token forKey:@"token"];
    if(_resetsInMillis != nil) [dict setObject:_resetsInMillis forKey:@"resetsInMillis"];
    if(_remainingCalls != nil) [dict setObject:_remainingCalls forKey:@"remainingCalls"];
    if(_expiresInMillis != nil) [dict setObject:_expiresInMillis forKey:@"expiresInMillis"];
    if(_totalRequests != nil) [dict setObject:_totalRequests forKey:@"totalRequests"];
    NSDictionary* output = [dict copy];
    return output;
}

@end

