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

-(void) addHeader:(NSString*) value
           forKey:(NSString*)key;

-(void) createUsersWithArrayInputWithCompletionBlock :(NSArray*) body 
        completionHandler:(void (^)(NSError *))completionBlock;
-(void) createUserWithCompletionBlock :(NIKUser*) body 
        completionHandler:(void (^)(NSError *))completionBlock;
-(void) createUsersWithListInputWithCompletionBlock :(NSArray*) body 
        completionHandler:(void (^)(NSError *))completionBlock;
-(void) updateUserWithCompletionBlock :(NSString*) username body:(NIKUser*) body 
        completionHandler:(void (^)(NSError *))completionBlock;
-(void) deleteUserWithCompletionBlock :(NSString*) username 
        completionHandler:(void (^)(NSError *))completionBlock;
-(void) getUserByNameWithCompletionBlock :(NSString*) username 
        completionHandler:(void (^)(NIKUser*, NSError *))completionBlock;
-(void) loginUserWithCompletionBlock :(NSString*) username password:(NSString*) password 
        completionHandler:(void (^)(NSString*, NSError *))completionBlock;
-(void) logoutUserWithCompletionBlock :
        completionHandler:(void (^)(NSError *))completionBlock;
@end
