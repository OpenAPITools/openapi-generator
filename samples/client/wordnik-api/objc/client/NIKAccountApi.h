#import <Foundation/Foundation.h>
#import "NIKApiInvoker.h"
#import "NIKApiTokenStatus.h"
#import "NIKWordList.h"
#import "NIKUser.h"
#import "NIKAuthenticationToken.h"


@interface NIKAccountApi: NSObject {

@private
    NSOperationQueue *_queue;
    NIKApiInvoker * _api;
}
@property(nonatomic, readonly) NSOperationQueue* queue;
@property(nonatomic, readonly) NIKApiInvoker* api;

-(void) addHeader:(NSString*) value
           forKey:(NSString*)key;

-(void) authenticateWithCompletionBlock :(NSString*) username password:(NSString*) password 
        completionHandler:(void (^)(NIKAuthenticationToken*, NSError *))completionBlock;
-(void) authenticatePostWithCompletionBlock :(NSString*) username body:(NSString*) body 
        completionHandler:(void (^)(NIKAuthenticationToken*, NSError *))completionBlock;
-(void) getWordListsForLoggedInUserWithCompletionBlock :(NSString*) auth_token skip:(NSNumber*) skip limit:(NSNumber*) limit 
        completionHandler:(void (^)(NSArray*, NSError *))completionBlock;
-(void) getApiTokenStatusWithCompletionBlock :(NSString*) api_key 
        completionHandler:(void (^)(NIKApiTokenStatus*, NSError *))completionBlock;
-(void) getLoggedInUserWithCompletionBlock :(NSString*) auth_token 
        completionHandler:(void (^)(NIKUser*, NSError *))completionBlock;
@end
