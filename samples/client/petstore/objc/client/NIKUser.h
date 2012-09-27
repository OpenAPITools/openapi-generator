#import <Foundation/Foundation.h>
#import "NIKSwaggerObject.h"

@interface NIKUser : NIKSwaggerObject {
@private
    NSNumber* __id; //NSNumber
    NSString* _lastName; //NSString
    NSString* _username; //NSString
    NSString* _phone; //NSString
    NSString* _email; //NSString
    NSNumber* _userStatus; //NSNumber
    NSString* _firstName; //NSString
    NSString* _password; //NSString
    }



@property(nonatomic) NSNumber* _id;
@property(nonatomic) NSString* lastName;
@property(nonatomic) NSString* username;
@property(nonatomic) NSString* phone;
@property(nonatomic) NSString* email;
@property(nonatomic) NSNumber* userStatus;
@property(nonatomic) NSString* firstName;
@property(nonatomic) NSString* password;
- (id) _id: (NSNumber*) _id
     lastName: (NSString*) lastName
     username: (NSString*) username
     phone: (NSString*) phone
     email: (NSString*) email
     userStatus: (NSNumber*) userStatus
     firstName: (NSString*) firstName
     password: (NSString*) password;

- (id) initWithValues: (NSDictionary*)dict;
- (NSDictionary*) asDictionary;


@end

