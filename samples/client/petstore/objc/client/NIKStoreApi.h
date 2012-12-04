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

-(void) addHeader:(NSString*)value forKey:(NSString*)key;

/**

 Find purchase order by ID
 For valid response try integer IDs with value <= 5. Anything above 5 or nonintegers will generate API errors
 @param orderId ID of pet that needs to be fetched
 */
-(void) getOrderByIdWithCompletionBlock :(NSString*) orderId 
        completionHandler: (void (^)(NIKOrder* output, NSError* error))completionBlock;

/**

 Delete purchase order by ID
 For valid response try integer IDs with value < 1000. Anything above 1000 or nonintegers will generate API errors
 @param orderId ID of the order that needs to be deleted
 */
-(void) deleteOrderWithCompletionBlock :(NSString*) orderId 
        completionHandler: (void (^)(NSError* error))completionBlock;

/**

 Place an order for a pet
 
 @param body order placed for purchasing the pet
 */
-(void) placeOrderWithCompletionBlock :(NIKOrder*) body 
        completionHandler: (void (^)(NSError* error))completionBlock;

@end
