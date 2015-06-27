#import <Foundation/Foundation.h>
#import "SwaggerClient.SWGPet.h"
#import "SwaggerClient.SWGFile.h"
#import "SWGObject.h"
#import "SWGApiClient.h"


@interface SWGPetApi: NSObject

@property(nonatomic, assign)SWGApiClient *apiClient;

-(instancetype) initWithApiClient:(SWGApiClient *)apiClient;
-(void) addHeader:(NSString*)value forKey:(NSString*)key;
-(unsigned long) requestQueueSize;
+(SWGPetApi*) apiWithHeader:(NSString*)headerValue key:(NSString*)key;
+(void) setBasePath:(NSString*)basePath;
+(NSString*) getBasePath;
///
///
/// Update an existing pet
/// 
///
/// @param body Pet object that needs to be added to the store
/// 
///
/// @return 
-(NSNumber*) updatePetWithCompletionBlock :(SWGPet*) body 
    
    
    completionHandler: (void (^)(NSError* error))completionBlock;


///
///
/// Add a new pet to the store
/// 
///
/// @param body Pet object that needs to be added to the store
/// 
///
/// @return 
-(NSNumber*) addPetWithCompletionBlock :(SWGPet*) body 
    
    
    completionHandler: (void (^)(NSError* error))completionBlock;


///
///
/// Finds Pets by status
/// Multiple status values can be provided with comma seperated strings
///
/// @param status Status values that need to be considered for filter
/// 
///
/// @return NSArray<SWGPet>*
-(NSNumber*) findPetsByStatusWithCompletionBlock :(NSArray*) status 
    
    completionHandler: (void (^)(NSArray<SWGPet>* output, NSError* error))completionBlock;
    


///
///
/// Finds Pets by tags
/// Muliple tags can be provided with comma seperated strings. Use tag1, tag2, tag3 for testing.
///
/// @param tags Tags to filter by
/// 
///
/// @return NSArray<SWGPet>*
-(NSNumber*) findPetsByTagsWithCompletionBlock :(NSArray*) tags 
    
    completionHandler: (void (^)(NSArray<SWGPet>* output, NSError* error))completionBlock;
    


///
///
/// Find pet by ID
/// Returns a pet when ID < 10.  ID > 10 or nonintegers will simulate API error conditions
///
/// @param petId ID of pet that needs to be fetched
/// 
///
/// @return SWGPet*
-(NSNumber*) getPetByIdWithCompletionBlock :(NSNumber*) petId 
    
    completionHandler: (void (^)(SWGPet* output, NSError* error))completionBlock;
    


///
///
/// Updates a pet in the store with form data
/// 
///
/// @param petId ID of pet that needs to be updated
/// @param name Updated name of the pet
/// @param status Updated status of the pet
/// 
///
/// @return 
-(NSNumber*) updatePetWithFormWithCompletionBlock :(NSString*) petId 
     name:(NSString*) name 
     status:(NSString*) status 
    
    
    completionHandler: (void (^)(NSError* error))completionBlock;


///
///
/// Deletes a pet
/// 
///
/// @param apiKey 
/// @param petId Pet id to delete
/// 
///
/// @return 
-(NSNumber*) deletePetWithCompletionBlock :(NSString*) apiKey 
     petId:(NSNumber*) petId 
    
    
    completionHandler: (void (^)(NSError* error))completionBlock;


///
///
/// uploads an image
/// 
///
/// @param petId ID of pet to update
/// @param additionalMetadata Additional data to pass to server
/// @param file file to upload
/// 
///
/// @return 
-(NSNumber*) uploadFileWithCompletionBlock :(NSNumber*) petId 
     additionalMetadata:(NSString*) additionalMetadata 
     file:(SWGFile*) file 
    
    
    completionHandler: (void (^)(NSError* error))completionBlock;



@end
