#import "NIKDate.h"
#import "NIKUser.h"

@implementation NIKUser

@synthesize _id = __id;
@synthesize username = _username;
@synthesize email = _email;
@synthesize status = _status;
@synthesize faceBookId = _faceBookId;
@synthesize userName = _userName;
@synthesize displayName = _displayName;
@synthesize password = _password;
- (id) _id: (NSNumber*) _id
       username: (NSString*) username
       email: (NSString*) email
       status: (NSNumber*) status
       faceBookId: (NSString*) faceBookId
       userName: (NSString*) userName
       displayName: (NSString*) displayName
       password: (NSString*) password
       {
          __id = _id;
          _username = username;
          _email = email;
          _status = status;
          _faceBookId = faceBookId;
          _userName = userName;
          _displayName = displayName;
          _password = password;
          return self;
       }

- (id) initWithValues: (NSDictionary*)dict
{
    __id = [dict objectForKey:@"id"];
    _username = [dict objectForKey:@"username"];
    _email = [dict objectForKey:@"email"];
    _status = [dict objectForKey:@"status"];
    _faceBookId = [dict objectForKey:@"faceBookId"];
    _userName = [dict objectForKey:@"userName"];
    _displayName = [dict objectForKey:@"displayName"];
    _password = [dict objectForKey:@"password"];
    return self;
}

-(NSDictionary*) asDictionary {
    NSMutableDictionary* dict = [[NSMutableDictionary alloc] init];
    if(__id != nil) [dict setObject:__id forKey:@"id"];
    if(_username != nil) [dict setObject:_username forKey:@"username"];
    if(_email != nil) [dict setObject:_email forKey:@"email"];
    if(_status != nil) [dict setObject:_status forKey:@"status"];
    if(_faceBookId != nil) [dict setObject:_faceBookId forKey:@"faceBookId"];
    if(_userName != nil) [dict setObject:_userName forKey:@"userName"];
    if(_displayName != nil) [dict setObject:_displayName forKey:@"displayName"];
    if(_password != nil) [dict setObject:_password forKey:@"password"];
    NSDictionary* output = [dict copy];
    return output;
}

@end

