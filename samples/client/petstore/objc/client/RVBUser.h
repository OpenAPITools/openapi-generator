#import <Foundation/Foundation.h>
#import "NIKSwaggerObject.h"

@interface RVBUser : NIKSwaggerObject

@property(nonatomic) NSNumber* _id;
@property(nonatomic) NSString* firstName;
@property(nonatomic) NSString* username;
@property(nonatomic) NSString* lastName;
@property(nonatomic) NSString* email;
@property(nonatomic) NSString* password;
@property(nonatomic) NSString* phone;
@property(nonatomic) NSNumber* userStatus;
- (id) _id: (NSNumber*) _id
     firstName: (NSString*) firstName
     username: (NSString*) username
     lastName: (NSString*) lastName
     email: (NSString*) email
     password: (NSString*) password
     phone: (NSString*) phone
     userStatus: (NSNumber*) userStatus;

- (id) initWithValues: (NSDictionary*)dict;
- (NSDictionary*) asDictionary;


@end

