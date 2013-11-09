#import <Foundation/Foundation.h>
#import "SWGObject.h"

@interface SWGUser : SWGObject

@property(nonatomic) NSNumber* _id;  /* Unique identifier for the user [optional]*/

@property(nonatomic) NSString* username;  /* Unique username [optional]*/

@property(nonatomic) NSString* firstName;  /* First name of the user [optional]*/

@property(nonatomic) NSString* lastName;  /* Last name of the user [optional]*/

@property(nonatomic) NSString* email;  /* Email address of the user [optional]*/

@property(nonatomic) NSString* password;  /* Password name of the user [optional]*/

@property(nonatomic) NSString* phone;  /* Phone number of the user [optional]*/

@property(nonatomic) NSNumber* userStatus;  /* User Status [optional]*/

- (id) _id: (NSNumber*) _id
     username: (NSString*) username
     firstName: (NSString*) firstName
     lastName: (NSString*) lastName
     email: (NSString*) email
     password: (NSString*) password
     phone: (NSString*) phone
     userStatus: (NSNumber*) userStatus;

- (id) initWithValues: (NSDictionary*)dict;
- (NSDictionary*) asDictionary;


@end

