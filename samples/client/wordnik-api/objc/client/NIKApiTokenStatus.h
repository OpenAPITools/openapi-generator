#import <Foundation/Foundation.h>
#import "NIKSwaggerObject.h"

@interface NIKApiTokenStatus : NIKSwaggerObject {
@private
    NSNumber* _valid; //NSNumber
    NSString* _token; //NSString
    NSNumber* _resetsInMillis; //NSNumber
    NSNumber* _remainingCalls; //NSNumber
    NSNumber* _expiresInMillis; //NSNumber
    NSNumber* _totalRequests; //NSNumber
    }



@property(nonatomic) NSNumber* valid;
@property(nonatomic) NSString* token;
@property(nonatomic) NSNumber* resetsInMillis;
@property(nonatomic) NSNumber* remainingCalls;
@property(nonatomic) NSNumber* expiresInMillis;
@property(nonatomic) NSNumber* totalRequests;
- (id) valid: (NSNumber*) valid
     token: (NSString*) token
     resetsInMillis: (NSNumber*) resetsInMillis
     remainingCalls: (NSNumber*) remainingCalls
     expiresInMillis: (NSNumber*) expiresInMillis
     totalRequests: (NSNumber*) totalRequests;

- (id) initWithValues: (NSDictionary*)dict;
- (NSDictionary*) asDictionary;


@end

