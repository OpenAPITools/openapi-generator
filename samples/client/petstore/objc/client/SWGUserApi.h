#import <Foundation/Foundation.h>
#import "SWGUser.h"


@interface SWGUserApi: NSObject

-(void) addHeader:(NSString*)value forKey:(NSString*)key;
-(unsigned long) requestQueueSize;
+(SWGUserApi*) apiWithHeader:(NSString*)headerValue key:(NSString*)key;
+(void) setBasePath:(NSString*)basePath;
+(NSString*) getBasePath;
/**

 Create user
 This can only be done by the logged in user.
 @param body Created user object
 */
-(NSNumber*) createUserWithCompletionBlock :(SWGUser*) body 
        completionHandler: (void (^)(NSError* error))completionBlock;

/**

 Creates list of users with given input array
 
 @param body List of user object
 */
-(NSNumber*) createUsersWithArrayInputWithCompletionBlock :(NSArray*) body 
        completionHandler: (void (^)(NSError* error))completionBlock;

/**

 Creates list of users with given list input
 
 @param body List of user object
 */
-(NSNumber*) createUsersWithListInputWithCompletionBlock :(NSArray*) body 
        completionHandler: (void (^)(NSError* error))completionBlock;

/**

 Updated user
 This can only be done by the logged in user.
 @param username name that need to be deleted
 @param body Updated user object
 */
-(NSNumber*) updateUserWithCompletionBlock :(NSString*) username 
        body:(SWGUser*) body 
        completionHandler: (void (^)(NSError* error))completionBlock;

/**

 Delete user
 This can only be done by the logged in user.
 @param username The name that needs to be deleted
 */
-(NSNumber*) deleteUserWithCompletionBlock :(NSString*) username 
        completionHandler: (void (^)(NSError* error))completionBlock;

/**

 Get user by user name
 
 @param username The name that needs to be fetched. Use user1 for testing.
 */
-(NSNumber*) getUserByNameWithCompletionBlock :(NSString*) username 
        completionHandler: (void (^)(SWGUser* output, NSError* error))completionBlock;

/**

 Logs user into the system
 
 @param username The user name for login
 @param password The password for login in clear text
 */
-(NSNumber*) loginUserWithCompletionBlock :(NSString*) username 
        password:(NSString*) password 
        completionHandler: (void (^)(NSString* output, NSError* error))completionBlock;

/**

 Logs out current logged in user session
 
 */
-(NSNumber*) logoutUserWithCompletionBlock :(void (^)(NSError* error))completionBlock;

@end
