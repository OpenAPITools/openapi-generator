#import <Foundation/Foundation.h>
#import "NIKApiInvoker.h"
#import "NIKPet.h"


@interface NIKPetApi: NSObject {

@private
    NSOperationQueue *_queue;
    NIKApiInvoker * _api;
}
@property(nonatomic, readonly) NSOperationQueue* queue;
@property(nonatomic, readonly) NIKApiInvoker* api;

-(void) addHeader:(NSString*)value forKey:(NSString*)key;

/**

 Find pet by ID
 Returns a pet based on ID
 @param petId ID of pet that needs to be fetched
 */
-(void) getPetByIdWithCompletionBlock :(NSString*) petId 
        completionHandler: (void (^)(NIKPet* output, NSError* error))completionBlock;

/**

 Add a new pet to the store
 
 @param body Pet object that needs to be added to the store
 */
-(void) addPetWithCompletionBlock :(NIKPet*) body 
        completionHandler: (void (^)(NSError* error))completionBlock;

/**

 Update an existing pet
 
 @param body Pet object that needs to be updated in the store
 */
-(void) updatePetWithCompletionBlock :(NIKPet*) body 
        completionHandler: (void (^)(NSError* error))completionBlock;

/**

 Finds Pets by status
 Multiple status values can be provided with comma seperated strings
 @param status Status values that need to be considered for filter
 */
-(void) findPetsByStatusWithCompletionBlock :(NSString*) status 
        completionHandler: (void (^)(NSArray* output, NSError* error))completionBlock;

/**

 Finds Pets by tags
 Muliple tags can be provided with comma seperated strings. Use tag1, tag2, tag3 for testing.
 @param tags Tags to filter by
 */
-(void) findPetsByTagsWithCompletionBlock :(NSString*) tags 
        completionHandler: (void (^)(NSArray* output, NSError* error))completionBlock;

@end
