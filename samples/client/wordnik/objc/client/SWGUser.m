#import "SWGDate.h"
#import "SWGUser.h"

@implementation SWGUser

-(id)_id: (NSNumber*) _id
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

-(id) initWithValues:(NSDictionary*)dict
{
    self = [super init];
    if(self) {
        __id = dict[@"id"];
        
        _username = dict[@"username"];
        
        _email = dict[@"email"];
        
        _status = dict[@"status"];
        
        _faceBookId = dict[@"faceBookId"];
        
        _userName = dict[@"userName"];
        
        _displayName = dict[@"displayName"];
        
        _password = dict[@"password"];
        
        
    }
    return self;
}

-(NSDictionary*) asDictionary {
    NSMutableDictionary* dict = [[NSMutableDictionary alloc] init];
    
    
            if(__id != nil) dict[@"id"] = __id ;
        
    
    
            if(_username != nil) dict[@"username"] = _username ;
        
    
    
            if(_email != nil) dict[@"email"] = _email ;
        
    
    
            if(_status != nil) dict[@"status"] = _status ;
        
    
    
            if(_faceBookId != nil) dict[@"faceBookId"] = _faceBookId ;
        
    
    
            if(_userName != nil) dict[@"userName"] = _userName ;
        
    
    
            if(_displayName != nil) dict[@"displayName"] = _displayName ;
        
    
    
            if(_password != nil) dict[@"password"] = _password ;
        
    

    NSDictionary* output = [dict copy];
    return output;
}

@end
