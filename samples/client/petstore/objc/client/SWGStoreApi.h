#import <Foundation/Foundation.h>
#import "SWGOrder.h"


@interface SWGStoreApi: NSObject

-(void) addHeader:(NSString*)value forKey:(NSString*)key;
-(unsigned long) requestQueueSize;
+(SWGStoreApi*) apiWithHeader:(NSString*)headerValue key:(NSString*)key;
+(void) setBasePath:(NSString*)basePath;
+(NSString*) getBasePath;
/**

 Find purchase order by ID
 For valid response try integer IDs with value <= 5. Anything above 5 or nonintegers will generate API errors
 @param orderId ID of pet that needs to be fetched
 */
-(NSNumber*) getOrderByIdWithCompletionBlock :(NSString*) orderId 
        completionHandler: (void (^)(SWGOrder* output, NSError* error))completionBlock;

/**

 Delete purchase order by ID
 For valid response try integer IDs with value < 1000.  Anything above 1000 or nonintegers will generate API errors
 @param orderId ID of the order that needs to be deleted
 */
-(NSNumber*) deleteOrderWithCompletionBlock :(NSString*) orderId 
        completionHandler: (void (^)(NSError* error))completionBlock;

/**

 Place an order for a pet
 
 @param body order placed for purchasing the pet
 */
-(NSNumber*) placeOrderWithCompletionBlock :(SWGOrder*) body 
        completionHandler: (void (^)(NSError* error))completionBlock;

@end
