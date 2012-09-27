#import <Foundation/Foundation.h>
#import "NIKSwaggerObject.h"

@interface NIKAuthenticationToken : NIKSwaggerObject {
@private
    NSString* _token; //NSString
    NSNumber* _userId; //NSNumber
    NSString* _userSignature; //NSString
    }



@property(nonatomic) NSString* token;
@property(nonatomic) NSNumber* userId;
@property(nonatomic) NSString* userSignature;
- (id) token: (NSString*) token
     userId: (NSNumber*) userId
     userSignature: (NSString*) userSignature;

- (id) initWithValues: (NSDictionary*)dict;
- (NSDictionary*) asDictionary;


@end

