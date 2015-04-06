#import <Foundation/Foundation.h>
#import "SWGObject.h"


@protocol SWGUser
@end
  
@interface SWGUser : SWGObject


@property(nonatomic) NSNumber<Optional>* _id;

@property(nonatomic) NSString<Optional>* username;

@property(nonatomic) NSString<Optional>* firstName;

@property(nonatomic) NSString<Optional>* lastName;

@property(nonatomic) NSString<Optional>* email;

@property(nonatomic) NSString<Optional>* password;

@property(nonatomic) NSString<Optional>* phone;
/* User Status [optional]
 */
@property(nonatomic) NSNumber<Optional>* userStatus;

@end
