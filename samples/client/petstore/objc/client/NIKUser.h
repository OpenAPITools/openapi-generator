#import <Foundation/Foundation.h>
#import "NIKSwaggerObject.h"

@interface NIKUser : NIKSwaggerObject

@property(nonatomic) NSNumber* _id;
@property(nonatomic) NSString* lastName;
@property(nonatomic) NSString* phone;
@property(nonatomic) NSString* username;
@property(nonatomic) NSString* email;
@property(nonatomic) NSNumber* userStatus;
@property(nonatomic) NSString* firstName;
@property(nonatomic) NSString* password;
- (id) _id: (NSNumber*) _id
     lastName: (NSString*) lastName
     phone: (NSString*) phone
     username: (NSString*) username
     email: (NSString*) email
     userStatus: (NSNumber*) userStatus
     firstName: (NSString*) firstName
     password: (NSString*) password;

- (id) initWithValues: (NSDictionary*)dict;
- (NSDictionary*) asDictionary;


@end

