#import <Foundation/Foundation.h>
#import "NIKApiInvoker.h"
#import "NIKWordList.h"


@interface NIKWordListsApi: NSObject {

@private
    NSOperationQueue *_queue;
    NIKApiInvoker * _api;
}
@property(nonatomic, readonly) NSOperationQueue* queue;
@property(nonatomic, readonly) NIKApiInvoker* api;

-(void) addHeader:(NSString*) value
           forKey:(NSString*)key;

-(void) createWordListWithCompletionBlock :(NIKWordList*) body auth_token:(NSString*) auth_token 
        completionHandler:(void (^)(NIKWordList*, NSError *))completionBlock;
@end
