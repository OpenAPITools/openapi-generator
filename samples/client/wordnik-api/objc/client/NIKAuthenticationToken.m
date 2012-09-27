#import "NIKDate.h"
#import "NIKAuthenticationToken.h"

@implementation NIKAuthenticationToken

@synthesize token = _token;
@synthesize userId = _userId;
@synthesize userSignature = _userSignature;
- (id) token: (NSString*) token
       userId: (NSNumber*) userId
       userSignature: (NSString*) userSignature
       {
          _token = token;
          _userId = userId;
          _userSignature = userSignature;
          return self;
       }

- (id) initWithValues: (NSDictionary*)dict
{
    _token = [dict objectForKey:@"token"];
    _userId = [dict objectForKey:@"userId"];
    _userSignature = [dict objectForKey:@"userSignature"];
    return self;
}

-(NSDictionary*) asDictionary {
    NSMutableDictionary* dict = [[NSMutableDictionary alloc] init];
    if(_token != nil) [dict setObject:_token forKey:@"token"];
    if(_userId != nil) [dict setObject:_userId forKey:@"userId"];
    if(_userSignature != nil) [dict setObject:_userSignature forKey:@"userSignature"];
    NSDictionary* output = [dict copy];
    return output;
}

@end

