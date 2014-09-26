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
    userStatus: (NSNumber*) userStatus { 
    
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
    if(__id != nil)
        dict[@"id"] = [(SWGObject*)__id asDictionary];
    if(_username != nil)
        dict[@"username"] = [(SWGObject*)_username asDictionary];
    if(_firstName != nil)
        dict[@"firstName"] = [(SWGObject*)_firstName asDictionary];
    if(_lastName != nil)
        dict[@"lastName"] = [(SWGObject*)_lastName asDictionary];
    if(_email != nil)
        dict[@"email"] = [(SWGObject*)_email asDictionary];
    if(_password != nil)
        dict[@"password"] = [(SWGObject*)_password asDictionary];
    if(_phone != nil)
        dict[@"phone"] = [(SWGObject*)_phone asDictionary];
    if(_userStatus != nil)
        dict[@"userStatus"] = [(SWGObject*)_userStatus asDictionary];
    
    NSDictionary* output = [dict copy];
    return output;
}

@end
