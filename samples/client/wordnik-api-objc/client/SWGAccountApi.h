#import <Foundation/Foundation.h>
#import "SWGUser.h"
#import "SWGWordList.h"
#import "SWGApiTokenStatus.h"
#import "SWGAuthenticationToken.h"



@interface SWGAccountApi: NSObject

-(void) addHeader:(NSString*)value forKey:(NSString*)key;
-(unsigned long) requestQueueSize;
+(SWGAccountApi*) apiWithHeader:(NSString*)headerValue key:(NSString*)key;
+(void) setBasePath:(NSString*)basePath;
+(NSString*) getBasePath;
/**

 Authenticates a User
 
 @param username A confirmed Wordnik username
 @param password The user's password
 */
-(NSNumber*) authenticateWithCompletionBlock :(NSString*) username 
        password:(NSString*) password 
        completionHandler: (void (^)(SWGAuthenticationToken* output, NSError* error))completionBlock;

/**

 Authenticates a user
 
 @param username A confirmed Wordnik username
 @param body The user's password
 */
-(NSNumber*) authenticatePostWithCompletionBlock :(NSString*) username 
        body:(NSString*) body 
        completionHandler: (void (^)(SWGAuthenticationToken* output, NSError* error))completionBlock;

/**

 Fetches WordList objects for the logged-in user.
 
 @param auth_token auth_token of logged-in user
 @param skip Results to skip
 @param limit Maximum number of results to return
 */
-(NSNumber*) getWordListsForLoggedInUserWithCompletionBlock :(NSString*) auth_token 
        skip:(NSNumber*) skip 
        limit:(NSNumber*) limit 
        completionHandler: (void (^)(NSArray* output, NSError* error))completionBlock;

/**

 Returns usage statistics for the API account.
 
 @param api_key Wordnik authentication token
 */
-(NSNumber*) getApiTokenStatusWithCompletionBlock :(NSString*) api_key 
        completionHandler: (void (^)(SWGApiTokenStatus* output, NSError* error))completionBlock;

/**

 Returns the logged-in User
 Requires a valid auth_token to be set.
 @param auth_token The auth token of the logged-in user, obtained by calling /account.{format}/authenticate/{username} (described above)
 */
-(NSNumber*) getLoggedInUserWithCompletionBlock :(NSString*) auth_token 
        completionHandler: (void (^)(SWGUser* output, NSError* error))completionBlock;

@end
