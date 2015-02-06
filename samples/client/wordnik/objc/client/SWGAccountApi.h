#import <Foundation/Foundation.h>


@interface SWGAccountApi: NSObject

-(void) addHeader:(NSString*)value forKey:(NSString*)key;
-(unsigned long) requestQueueSize;
+(SWGAccountApi*) apiWithHeader:(NSString*)headerValue key:(NSString*)key;
+(void) setBasePath:(NSString*)basePath;
+(NSString*) getBasePath;
/**

 Returns usage statistics for the API account.
 

 
 @param api_key Wordnik authentication token
 

 */
-(NSNumber*) getApiTokenStatusWithCompletionBlock :(NSString*) api_key 
        
                completionHandler: (void (^)(NSError* error))completionBlock;


/**

 Authenticates a User
 

 
 @param username A confirmed Wordnik username
 
 @param password The user&#39;s password
 

 */
-(NSNumber*) authenticateWithCompletionBlock :(NSString*) username          password:(NSString*) password 
        
                completionHandler: (void (^)(NSError* error))completionBlock;


/**

 Authenticates a user
 

 
 @param username A confirmed Wordnik username
 
 @param body The user&#39;s password
 

 */
-(NSNumber*) authenticatePostWithCompletionBlock :(NSString*) username          body:(NSString*) body 
        
                completionHandler: (void (^)(NSError* error))completionBlock;


/**

 Returns the logged-in User
 
 Requires a valid auth_token to be set.
 

 
 @param auth_token The auth token of the logged-in user, obtained by calling /account.json/authenticate/{username} (described above)
 

 */
-(NSNumber*) getLoggedInUserWithCompletionBlock :(NSString*) auth_token 
        
                completionHandler: (void (^)(NSError* error))completionBlock;


/**

 Fetches WordList objects for the logged-in user.
 

 
 @param auth_token auth_token of logged-in user
 
 @param skip Results to skip
 
 @param limit Maximum number of results to return
 

 */
-(NSNumber*) getWordListsForLoggedInUserWithCompletionBlock :(NSString*) auth_token          skip:(NSNumber*) skip          limit:(NSNumber*) limit 
        
                completionHandler: (void (^)(NSError* error))completionBlock;



@end