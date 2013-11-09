#import <Foundation/Foundation.h>
#import "SWGPet.h"
#import "SWGFile.h"


@interface SWGPetApi: NSObject

-(void) addHeader:(NSString*)value forKey:(NSString*)key;
-(unsigned long) requestQueueSize;
+(SWGPetApi*) apiWithHeader:(NSString*)headerValue key:(NSString*)key;
+(void) setBasePath:(NSString*)basePath;
+(NSString*) getBasePath;
/**

 Find pet by ID
 Returns a pet based on ID
 @param petId ID of pet that needs to be fetched
 */
-(NSNumber*) getPetByIdWithCompletionBlock :(NSNumber*) petId 
        completionHandler: (void (^)(SWGPet* output, NSError* error))completionBlock;

/**

 Deletes a pet
 
 @param petId Pet id to delete
 */
-(NSNumber*) deletePetWithCompletionBlock :(NSString*) petId 
        completionHandler: (void (^)(NSError* error))completionBlock;

/**

 partial updates to a pet
 
 @param petId ID of pet that needs to be fetched
 @param body Pet object that needs to be added to the store
 */
-(NSNumber*) partialUpdateWithCompletionBlock :(NSString*) petId 
        body:(SWGPet*) body 
        completionHandler: (void (^)(NSArray* output, NSError* error))completionBlock;

/**

 Updates a pet in the store with form data
 
 @param petId ID of pet that needs to be updated
 @param name Updated name of the pet
 @param status Updated status of the pet
 */
-(NSNumber*) updatePetWithFormWithCompletionBlock :(NSString*) petId 
        name:(NSString*) name 
        status:(NSString*) status 
        completionHandler: (void (^)(NSError* error))completionBlock;

/**

 uploads an image
 
 @param additionalMetadata Additional data to pass to server
 @param body file to upload
 */
-(NSNumber*) uploadFileWithCompletionBlock :(NSString*) additionalMetadata 
        body:(SWGFile*) body 
        completionHandler: (void (^)(NSError* error))completionBlock;

/**

 Add a new pet to the store
 
 @param body Pet object that needs to be added to the store
 */
-(NSNumber*) addPetWithCompletionBlock :(SWGPet*) body 
        completionHandler: (void (^)(NSError* error))completionBlock;

/**

 Update an existing pet
 
 @param body Pet object that needs to be updated in the store
 */
-(NSNumber*) updatePetWithCompletionBlock :(SWGPet*) body 
        completionHandler: (void (^)(NSError* error))completionBlock;

/**

 Finds Pets by status
 Multiple status values can be provided with comma seperated strings
 @param status Status values that need to be considered for filter
 */
-(NSNumber*) findPetsByStatusWithCompletionBlock :(NSString*) status 
        completionHandler: (void (^)(NSArray* output, NSError* error))completionBlock;

/**

 Finds Pets by tags
 Muliple tags can be provided with comma seperated strings. Use tag1, tag2, tag3 for testing.
 @param tags Tags to filter by
 */
-(NSNumber*) findPetsByTagsWithCompletionBlock :(NSString*) tags 
        completionHandler: (void (^)(NSArray* output, NSError* error))completionBlock;

@end
