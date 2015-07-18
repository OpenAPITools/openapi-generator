#import <Foundation/Foundation.h>
#import "SWGObject.h"


@protocol SWGUser
@end
  
@interface SWGUser : SWGObject


@property(nonatomic) NSNumber* _id;

@property(nonatomic) NSString* username;

@property(nonatomic) NSString* firstName;

@property(nonatomic) NSString* lastName;

@property(nonatomic) NSString* email;

@property(nonatomic) NSString* password;

@property(nonatomic) NSString* phone;
/* User Status [optional]
 */
@property(nonatomic) NSNumber* userStatus;

@end
