#import <Foundation/Foundation.h>
#import "NIKApiInvoker.h"
#import "NIKUser.h"


@interface NIKUserApi: NSObject {

@private
    NSOperationQueue *_queue;
    NIKApiInvoker * _api;
}
@property(nonatomic, readonly) NSOperationQueue* queue;
@property(nonatomic, readonly) NIKApiInvoker* api;

-(void) addHeader:(NSString*)value forKey:(NSString*)key;

/**

 Creates list of users with given input array
 
 @param body List of user object
 */
-(void) createUsersWithArrayInputWithCompletionBlock :(NSArray*) body 
        completionHandler: (void (^)(NSError* error))completionBlock;

/**

 Create user
 This can only be done by the logged in user.
 @param body Created user object
 */
-(void) createUserWithCompletionBlock :(NIKUser*) body 
        completionHandler: (void (^)(NSError* error))completionBlock;

/**

 Creates list of users with given list input
 
 @param body List of user object
 */
-(void) createUsersWithListInputWithCompletionBlock :(NSArray*) body 
        completionHandler: (void (^)(NSError* error))completionBlock;

/**

 Updated user
 This can only be done by the logged in user.
 @param username name that need to be deleted
 @param body Updated user object
 */
-(void) updateUserWithCompletionBlock :(NSString*) username 
        body:(NIKUser*) body 
        completionHandler: (void (^)(NSError* error))completionBlock;

/**

 Delete user
 This can only be done by the logged in user.
 @param username The name that needs to be deleted
 */
-(void) deleteUserWithCompletionBlock :(NSString*) username 
        completionHandler: (void (^)(NSError* error))completionBlock;

/**

 Get user by user name
 
 @param username The name that needs to be fetched. Use user1 for testing.
 */
-(void) getUserByNameWithCompletionBlock :(NSString*) username 
        completionHandler: (void (^)(NIKUser* output, NSError* error))completionBlock;

/**

 Logs user into the system
 
 @param username The user name for login
 @param password The password for login in clear text
 */
-(void) loginUserWithCompletionBlock :(NSString*) username 
        password:(NSString*) password 
        completionHandler: (void (^)(NSString* output, NSError* error))completionBlock;

/**

 Logs out current logged in user session
 
 */
-(void) logoutUserWithCompletionBlock :(void (^)(NSError* error))completionBlock;

@end
