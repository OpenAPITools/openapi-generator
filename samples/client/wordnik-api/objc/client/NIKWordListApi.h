#import <Foundation/Foundation.h>
#import "NIKApiInvoker.h"
#import "NIKWordList.h"
#import "NIKStringValue.h"
#import "NIKWordListWord.h"


@interface NIKWordListApi: NSObject {

@private
    NSOperationQueue *_queue;
    NIKApiInvoker * _api;
}
@property(nonatomic, readonly) NSOperationQueue* queue;
@property(nonatomic, readonly) NIKApiInvoker* api;

-(void) addHeader:(NSString*) value
           forKey:(NSString*)key;

-(void) updateWordListWithCompletionBlock :(NSString*) permalink body:(NIKWordList*) body auth_token:(NSString*) auth_token 
        completionHandler:(void (^)(NSError *))completionBlock;
-(void) deleteWordListWithCompletionBlock :(NSString*) permalink auth_token:(NSString*) auth_token 
        completionHandler:(void (^)(NSError *))completionBlock;
-(void) getWordListByPermalinkWithCompletionBlock :(NSString*) permalink auth_token:(NSString*) auth_token 
        completionHandler:(void (^)(NIKWordList*, NSError *))completionBlock;
-(void) addWordsToWordListWithCompletionBlock :(NSString*) permalink body:(NSArray*) body auth_token:(NSString*) auth_token 
        completionHandler:(void (^)(NSError *))completionBlock;
-(void) getWordListWordsWithCompletionBlock :(NSString*) permalink auth_token:(NSString*) auth_token sortBy:(NSString*) sortBy sortOrder:(NSString*) sortOrder skip:(NSNumber*) skip limit:(NSNumber*) limit 
        completionHandler:(void (^)(NSArray*, NSError *))completionBlock;
-(void) deleteWordsFromWordListWithCompletionBlock :(NSString*) permalink body:(NSArray*) body auth_token:(NSString*) auth_token 
        completionHandler:(void (^)(NSError *))completionBlock;
@end
