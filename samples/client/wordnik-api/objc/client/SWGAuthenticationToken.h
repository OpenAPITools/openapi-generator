#import <Foundation/Foundation.h>
#import "SWGObject.h"


@interface SWGAuthenticationToken : SWGObject

@property(nonatomic) NSString* token;  

@property(nonatomic) NSNumber* userId;  

@property(nonatomic) NSString* userSignature;  

- (id) token: (NSString*) token
     userId: (NSNumber*) userId
     userSignature: (NSString*) userSignature;

- (id) initWithValues: (NSDictionary*)dict;
- (NSDictionary*) asDictionary;


@end

