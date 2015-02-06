#import <Foundation/Foundation.h>
#import "SWGWordList.h"
#import "SWGStringValue.h"
#import "SWGObject.h"


@interface SWGWordListApi: NSObject

-(void) addHeader:(NSString*)value forKey:(NSString*)key;
-(unsigned long) requestQueueSize;
+(SWGWordListApi*) apiWithHeader:(NSString*)headerValue key:(NSString*)key;
+(void) setBasePath:(NSString*)basePath;
+(NSString*) getBasePath;
/**

 Fetches a WordList by ID
 

 
 @param permalink permalink of WordList to fetch
 
 @param auth_token The auth token of the logged-in user, obtained by calling /account.json/authenticate/{username} (described above)
 

 return type: SWGWordList*
 */
-(NSNumber*) getWordListByPermalinkWithCompletionBlock :(NSString*) permalink 
     auth_token:(NSString*) auth_token 
    
    completionHandler: (void (^)(SWGWordList* output, NSError* error))completionBlock;
    


/**

 Updates an existing WordList
 

 
 @param permalink permalink of WordList to update
 
 @param body Updated WordList
 
 @param auth_token The auth token of the logged-in user, obtained by calling /account.json/authenticate/{username} (described above)
 

 return type: 
 */
-(NSNumber*) updateWordListWithCompletionBlock :(NSString*) permalink 
     body:(SWGWordList*) body 
     auth_token:(NSString*) auth_token 
    
    
    completionHandler: (void (^)(NSError* error))completionBlock;


/**

 Deletes an existing WordList
 

 
 @param permalink ID of WordList to delete
 
 @param auth_token The auth token of the logged-in user, obtained by calling /account.json/authenticate/{username} (described above)
 

 return type: 
 */
-(NSNumber*) deleteWordListWithCompletionBlock :(NSString*) permalink 
     auth_token:(NSString*) auth_token 
    
    
    completionHandler: (void (^)(NSError* error))completionBlock;


/**

 Removes words from a WordList
 

 
 @param permalink permalink of WordList to use
 
 @param body Words to remove from WordList
 
 @param auth_token The auth token of the logged-in user, obtained by calling /account.json/authenticate/{username} (described above)
 

 return type: 
 */
-(NSNumber*) deleteWordsFromWordListWithCompletionBlock :(NSString*) permalink 
     body:(NSArray*) body 
     auth_token:(NSString*) auth_token 
    
    
    completionHandler: (void (^)(NSError* error))completionBlock;


/**

 Fetches words in a WordList
 

 
 @param permalink ID of WordList to use
 
 @param sortBy Field to sort by
 
 @param sortOrder Direction to sort
 
 @param skip Results to skip
 
 @param limit Maximum number of results to return
 
 @param auth_token The auth token of the logged-in user, obtained by calling /account.json/authenticate/{username} (described above)
 

 return type: 
 */
-(NSNumber*) getWordListWordsWithCompletionBlock :(NSString*) permalink 
     sortBy:(NSString*) sortBy 
     sortOrder:(NSString*) sortOrder 
     skip:(NSNumber*) skip 
     limit:(NSNumber*) limit 
     auth_token:(NSString*) auth_token 
    
    
    completionHandler: (void (^)(NSError* error))completionBlock;


/**

 Adds words to a WordList
 

 
 @param permalink permalink of WordList to user
 
 @param body Array of words to add to WordList
 
 @param auth_token The auth token of the logged-in user, obtained by calling /account.json/authenticate/{username} (described above)
 

 return type: 
 */
-(NSNumber*) addWordsToWordListWithCompletionBlock :(NSString*) permalink 
     body:(NSArray*) body 
     auth_token:(NSString*) auth_token 
    
    
    completionHandler: (void (^)(NSError* error))completionBlock;



@end