#import "NIKDate.h"
#import "NIKUser.h"

@implementation NIKUser

@synthesize _id = __id;
@synthesize lastName = _lastName;
@synthesize username = _username;
@synthesize phone = _phone;
@synthesize email = _email;
@synthesize userStatus = _userStatus;
@synthesize firstName = _firstName;
@synthesize password = _password;
- (id) _id: (NSNumber*) _id
       lastName: (NSString*) lastName
       username: (NSString*) username
       phone: (NSString*) phone
       email: (NSString*) email
       userStatus: (NSNumber*) userStatus
       firstName: (NSString*) firstName
       password: (NSString*) password
       {
          __id = _id;
          _lastName = lastName;
          _username = username;
          _phone = phone;
          _email = email;
          _userStatus = userStatus;
          _firstName = firstName;
          _password = password;
          return self;
       }

- (id) initWithValues: (NSDictionary*)dict
{
    __id = [dict objectForKey:@"id"];
    _lastName = [dict objectForKey:@"lastName"];
    _username = [dict objectForKey:@"username"];
    _phone = [dict objectForKey:@"phone"];
    _email = [dict objectForKey:@"email"];
    _userStatus = [dict objectForKey:@"userStatus"];
    _firstName = [dict objectForKey:@"firstName"];
    _password = [dict objectForKey:@"password"];
    return self;
}

-(NSDictionary*) asDictionary {
    NSMutableDictionary* dict = [[NSMutableDictionary alloc] init];
    if(__id != nil) [dict setObject:__id forKey:@"id"];
    if(_lastName != nil) [dict setObject:_lastName forKey:@"lastName"];
    if(_username != nil) [dict setObject:_username forKey:@"username"];
    if(_phone != nil) [dict setObject:_phone forKey:@"phone"];
    if(_email != nil) [dict setObject:_email forKey:@"email"];
    if(_userStatus != nil) [dict setObject:_userStatus forKey:@"userStatus"];
    if(_firstName != nil) [dict setObject:_firstName forKey:@"firstName"];
    if(_password != nil) [dict setObject:_password forKey:@"password"];
    NSDictionary* output = [dict copy];
    return output;
}

@end

