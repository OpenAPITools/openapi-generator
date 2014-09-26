#import <Foundation/Foundation.h>
#import "SWGPet.h"


@interface SWGPetApi: NSObject

-(void) addHeader:(NSString*)value forKey:(NSString*)key;
-(unsigned long) requestQueueSize;
+(SWGPetApi*) apiWithHeader:(NSString*)headerValue key:(NSString*)key;
+(void) setBasePath:(NSString*)basePath;
+(NSString*) getBasePath;
/**

 Update an existing pet
 

 @param body Pet object that needs to be added to the store
 
 */
-(NSNumber*) updatePetWithCompletionBlock : (SWGPet*) body

    completionHandler: (void (^)(NSError* error))completionBlock;
/**

 Add a new pet to the store
 

 @param body Pet object that needs to be added to the store
 
 */
-(NSNumber*) addPetWithCompletionBlock : (SWGPet*) body

    completionHandler: (void (^)(NSError* error))completionBlock;
/**

 Finds Pets by status
 Multiple status values can be provided with comma seperated strings

 @param status Status values that need to be considered for filter
 
 */
-(NSNumber*) findPetsByStatusWithCompletionBlock : (NSArray*) status
    completionHandler: (void (^)(NSArray* output, NSError* error))completionBlock;

/**

 Finds Pets by tags
 Muliple tags can be provided with comma seperated strings. Use tag1, tag2, tag3 for testing.

 @param tags Tags to filter by
 
 */
-(NSNumber*) findPetsByTagsWithCompletionBlock : (NSArray*) tags
    completionHandler: (void (^)(NSArray* output, NSError* error))completionBlock;

/**

 Find pet by ID
 Returns a pet when ID < 10.  ID > 10 or nonintegers will simulate API error conditions

 @param petId ID of pet that needs to be fetched
 
 */
-(NSNumber*) getPetByIdWithCompletionBlock : (NSNumber*) petId
    completionHandler: (void (^)(SWGPet* output, NSError* error))completionBlock;

/**

 Deletes a pet
 

 @param petId Pet id to delete
 
 */
-(NSNumber*) deletePetWithCompletionBlock : (NSNumber*) petId

    completionHandler: (void (^)(NSError* error))completionBlock;
@end