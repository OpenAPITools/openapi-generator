#import "NIKDate.h"
#import "NIKUser.h"

@implementation NIKUser

-(id)_id: (NSNumber*) _id
    lastName: (NSString*) lastName
    phone: (NSString*) phone
    username: (NSString*) username
    email: (NSString*) email
    userStatus: (NSNumber*) userStatus
    firstName: (NSString*) firstName
    password: (NSString*) password
{
  __id = _id;
  _lastName = lastName;
  _phone = phone;
  _username = username;
  _email = email;
  _userStatus = userStatus;
  _firstName = firstName;
  _password = password;
  return self;
}

-(id) initWithValues:(NSDictionary*)dict
{
    self = [super init];
    if(self) {
        __id = dict[@"id"]; 
        _lastName = dict[@"lastName"]; 
        _phone = dict[@"phone"]; 
        _username = dict[@"username"]; 
        _email = dict[@"email"]; 
        _userStatus = dict[@"userStatus"]; 
        _firstName = dict[@"firstName"]; 
        _password = dict[@"password"]; 
        

    }
    return self;
}

-(NSDictionary*) asDictionary {
    NSMutableDictionary* dict = [[NSMutableDictionary alloc] init];
    if(__id != nil) dict[@"id"] = __id ;
    if(_lastName != nil) dict[@"lastName"] = _lastName ;
    if(_phone != nil) dict[@"phone"] = _phone ;
    if(_username != nil) dict[@"username"] = _username ;
    if(_email != nil) dict[@"email"] = _email ;
    if(_userStatus != nil) dict[@"userStatus"] = _userStatus ;
    if(_firstName != nil) dict[@"firstName"] = _firstName ;
    if(_password != nil) dict[@"password"] = _password ;
    NSDictionary* output = [dict copy];
    return output;
}

@end

