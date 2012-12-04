#import <Foundation/Foundation.h>
#import "NIKSwaggerObject.h"

@interface NIKUser : NIKSwaggerObject {
@private
    NSNumber* __id; //NSNumber
    NSString* _username; //NSString
    NSString* _email; //NSString
    NSNumber* _status; //NSNumber
    NSString* _faceBookId; //NSString
    NSString* _userName; //NSString
    NSString* _displayName; //NSString
    NSString* _password; //NSString
    }



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

