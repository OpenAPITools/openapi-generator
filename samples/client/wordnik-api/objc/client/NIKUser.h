#import <Foundation/Foundation.h>
#import "NIKSwaggerObject.h"

@interface NIKUser : NIKSwaggerObject {
@private
    NSNumber* __id; //NSNumber
    NSString* _username; //NSString
    NSNumber* _status; //NSNumber
    NSString* _email; //NSString
    NSString* _faceBookId; //NSString
    NSString* _userName; //NSString
    NSString* _displayName; //NSString
    NSString* _password; //NSString
    }



@property(nonatomic) NSNumber* _id;
@property(nonatomic) NSString* username;
@property(nonatomic) NSNumber* status;
@property(nonatomic) NSString* email;
@property(nonatomic) NSString* faceBookId;
@property(nonatomic) NSString* userName;
@property(nonatomic) NSString* displayName;
@property(nonatomic) NSString* password;
- (id) _id: (NSNumber*) _id
     username: (NSString*) username
     status: (NSNumber*) status
     email: (NSString*) email
     faceBookId: (NSString*) faceBookId
     userName: (NSString*) userName
     displayName: (NSString*) displayName
     password: (NSString*) password;

- (id) initWithValues: (NSDictionary*)dict;
- (NSDictionary*) asDictionary;


@end

