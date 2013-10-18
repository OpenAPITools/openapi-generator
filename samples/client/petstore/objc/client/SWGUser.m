#import "SWGDate.h"
#import "SWGUser.h"

@implementation SWGUser

-(id)_id: (NSNumber*) _id
    username: (NSString*) username
    firstName: (NSString*) firstName
    lastName: (NSString*) lastName
    email: (NSString*) email
    password: (NSString*) password
    phone: (NSString*) phone
    userStatus: (NSNumber*) userStatus
{
  __id = _id;
  _username = username;
  _firstName = firstName;
  _lastName = lastName;
  _email = email;
  _password = password;
  _phone = phone;
  _userStatus = userStatus;
  return self;
}

-(id) initWithValues:(NSDictionary*)dict
{
    self = [super init];
    if(self) {
        __id = dict[@"id"]; 
        _username = dict[@"username"]; 
        _firstName = dict[@"firstName"]; 
        _lastName = dict[@"lastName"]; 
        _email = dict[@"email"]; 
        _password = dict[@"password"]; 
        _phone = dict[@"phone"]; 
        _userStatus = dict[@"userStatus"]; 
        

    }
    return self;
}

-(NSDictionary*) asDictionary {
    NSMutableDictionary* dict = [[NSMutableDictionary alloc] init];
    if(__id != nil) dict[@"id"] = __id ;
        if(_username != nil) dict[@"username"] = _username ;
        if(_firstName != nil) dict[@"firstName"] = _firstName ;
        if(_lastName != nil) dict[@"lastName"] = _lastName ;
        if(_email != nil) dict[@"email"] = _email ;
        if(_password != nil) dict[@"password"] = _password ;
        if(_phone != nil) dict[@"phone"] = _phone ;
        if(_userStatus != nil) dict[@"userStatus"] = _userStatus ;
        NSDictionary* output = [dict copy];
    return output;
}

@end

