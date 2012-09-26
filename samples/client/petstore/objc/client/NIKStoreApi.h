#import <Foundation/Foundation.h>
#import "NIKApiInvoker.h"
#import "NIKOrder.h"


@interface NIKStoreApi: NSObject {

@private
    NSOperationQueue *_queue;
    NIKApiInvoker * _api;
}
@property(nonatomic, readonly) NSOperationQueue* queue;
@property(nonatomic, readonly) NIKApiInvoker* api;

-(void) addHeader:(NSString*) value
           forKey:(NSString*)key;

-(void) getOrderByIdWithCompletionBlock :(NSString*) orderId 
        completionHandler:(void (^)(NIKOrder*, NSError *))completionBlock;
-(void) deleteOrderWithCompletionBlock :(NSString*) orderId 
        completionHandler:(void (^)(NSError *))completionBlock;
-(void) placeOrderWithCompletionBlock :(NIKOrder*) body 
        completionHandler:(void (^)(NSError *))completionBlock;
@end
