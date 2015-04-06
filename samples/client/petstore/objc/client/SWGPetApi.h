#import <Foundation/Foundation.h>
#import "SWGPet.h"
#import "SWGFile.h"
#import "SWGObject.h"


@interface SWGPetApi: NSObject

-(void) addHeader:(NSString*)value forKey:(NSString*)key;
-(unsigned long) requestQueueSize;
+(SWGPetApi*) apiWithHeader:(NSString*)headerValue key:(NSString*)key;
+(void) setBasePath:(NSString*)basePath;
+(NSString*) getBasePath;
/**

 Update an existing pet
 

 @param body Pet object that needs to be added to the store
 

 return type: 
 */
-(NSNumber*) updatePetWithCompletionBlock :(SWGPet*) body 
    
    
    completionHandler: (void (^)(NSError* error))completionBlock;


/**

 Add a new pet to the store
 

 @param body Pet object that needs to be added to the store
 

 return type: 
 */
-(NSNumber*) addPetWithCompletionBlock :(SWGPet*) body 
    
    
    completionHandler: (void (^)(NSError* error))completionBlock;


/**

 Finds Pets by status
 Multiple status values can be provided with comma seperated strings

 @param status Status values that need to be considered for filter
 

 return type: NSArray<SWGPet>*
 */
-(NSNumber*) findPetsByStatusWithCompletionBlock :(NSArray*) status 
    
    completionHandler: (void (^)(NSArray<SWGPet>* output, NSError* error))completionBlock;
    


/**

 Finds Pets by tags
 Muliple tags can be provided with comma seperated strings. Use tag1, tag2, tag3 for testing.

 @param tags Tags to filter by
 

 return type: NSArray<SWGPet>*
 */
-(NSNumber*) findPetsByTagsWithCompletionBlock :(NSArray*) tags 
    
    completionHandler: (void (^)(NSArray<SWGPet>* output, NSError* error))completionBlock;
    


/**

 Find pet by ID
 Returns a pet when ID < 10.  ID > 10 or nonintegers will simulate API error conditions

 @param petId ID of pet that needs to be fetched
 

 return type: SWGPet*
 */
-(NSNumber*) getPetByIdWithCompletionBlock :(NSNumber*) petId 
    
    completionHandler: (void (^)(SWGPet* output, NSError* error))completionBlock;
    


/**

 Updates a pet in the store with form data
 

 @param petId ID of pet that needs to be updated
 @param name Updated name of the pet
 @param status Updated status of the pet
 

 return type: 
 */
-(NSNumber*) updatePetWithFormWithCompletionBlock :(NSString*) petId 
     name:(NSString*) name 
     status:(NSString*) status 
    
    
    completionHandler: (void (^)(NSError* error))completionBlock;


/**

 Deletes a pet
 

 @param api_key 
 @param petId Pet id to delete
 

 return type: 
 */
-(NSNumber*) deletePetWithCompletionBlock :(NSString*) api_key 
     petId:(NSNumber*) petId 
    
    
    completionHandler: (void (^)(NSError* error))completionBlock;


/**

 uploads an image
 

 @param petId ID of pet to update
 @param additionalMetadata Additional data to pass to server
 @param file file to upload
 

 return type: 
 */
-(NSNumber*) uploadFileWithCompletionBlock :(NSNumber*) petId 
     additionalMetadata:(NSString*) additionalMetadata 
     file:(SWGFile*) file 
    
    
    completionHandler: (void (^)(NSError* error))completionBlock;



@end