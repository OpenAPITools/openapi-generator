#import <Foundation/Foundation.h>
#import "SWGObject.h"


@interface SWGUser : SWGObject

@property(nonatomic) NSNumber* _id;  
@property(nonatomic) NSString* username;  
@property(nonatomic) NSString* email;  
@property(nonatomic) NSNumber* status;  
@property(nonatomic) NSString* faceBookId;  
@property(nonatomic) NSString* userName;  
@property(nonatomic) NSString* displayName;  
@property(nonatomic) NSString* password;  
- (id) _id: (NSNumber*) _id     
    username: (NSString*) username     
    email: (NSString*) email     
    status: (NSNumber*) status     
    faceBookId: (NSString*) faceBookId     
    userName: (NSString*) userName     
    displayName: (NSString*) displayName     
    password: (NSString*) password;
    

- (id) initWithValues: (NSDictionary*)dict;
- (NSDictionary*) asDictionary;

@end
