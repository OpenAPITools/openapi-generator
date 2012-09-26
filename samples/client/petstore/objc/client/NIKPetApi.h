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

-(void) addHeader:(NSString*) value
           forKey:(NSString*)key;

-(void) getPetByIdWithCompletionBlock :(NSString*) petId 
        completionHandler:(void (^)(NIKPet*, NSError *))completionBlock;
-(void) addPetWithCompletionBlock :(NIKPet*) body 
        completionHandler:(void (^)(NSError *))completionBlock;
-(void) updatePetWithCompletionBlock :(NIKPet*) body 
        completionHandler:(void (^)(NSError *))completionBlock;
-(void) findPetsByStatusWithCompletionBlock :(NSString*) status 
        completionHandler:(void (^)(NSArray*, NSError *))completionBlock;
-(void) findPetsByTagsWithCompletionBlock :(NSString*) tags 
        completionHandler:(void (^)(NSArray*, NSError *))completionBlock;
@end
