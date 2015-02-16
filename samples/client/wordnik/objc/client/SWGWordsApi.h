#import <Foundation/Foundation.h>
#import "SWGWordObject.h"
#import "SWGDefinitionSearchResults.h"
#import "SWGWordSearchResults.h"
#import "SWGWordOfTheDay.h"
#import "SWGObject.h"


@interface SWGWordsApi: NSObject

-(void) addHeader:(NSString*)value forKey:(NSString*)key;
-(unsigned long) requestQueueSize;
+(SWGWordsApi*) apiWithHeader:(NSString*)headerValue key:(NSString*)key;
+(void) setBasePath:(NSString*)basePath;
+(NSString*) getBasePath;
/**

 Returns a single random WordObject
 

 
 @param hasDictionaryDef Only return words with dictionary definitions
 
 @param includePartOfSpeech CSV part-of-speech values to include
 
 @param excludePartOfSpeech CSV part-of-speech values to exclude
 
 @param minCorpusCount Minimum corpus frequency for terms
 
 @param maxCorpusCount Maximum corpus frequency for terms
 
 @param minDictionaryCount Minimum dictionary count
 
 @param maxDictionaryCount Maximum dictionary count
 
 @param minLength Minimum word length
 
 @param maxLength Maximum word length
 

 return type: SWGWordObject*
 */
-(NSNumber*) getRandomWordWithCompletionBlock :(NSString*) hasDictionaryDef 
     includePartOfSpeech:(NSString*) includePartOfSpeech 
     excludePartOfSpeech:(NSString*) excludePartOfSpeech 
     minCorpusCount:(NSNumber*) minCorpusCount 
     maxCorpusCount:(NSNumber*) maxCorpusCount 
     minDictionaryCount:(NSNumber*) minDictionaryCount 
     maxDictionaryCount:(NSNumber*) maxDictionaryCount 
     minLength:(NSNumber*) minLength 
     maxLength:(NSNumber*) maxLength 
    
    completionHandler: (void (^)(SWGWordObject* output, NSError* error))completionBlock;
    


/**

 Returns an array of random WordObjects
 

 
 @param hasDictionaryDef Only return words with dictionary definitions
 
 @param includePartOfSpeech CSV part-of-speech values to include
 
 @param excludePartOfSpeech CSV part-of-speech values to exclude
 
 @param minCorpusCount Minimum corpus frequency for terms
 
 @param maxCorpusCount Maximum corpus frequency for terms
 
 @param minDictionaryCount Minimum dictionary count
 
 @param maxDictionaryCount Maximum dictionary count
 
 @param minLength Minimum word length
 
 @param maxLength Maximum word length
 
 @param sortBy Attribute to sort by
 
 @param sortOrder Sort direction
 
 @param limit Maximum number of results to return
 

 return type: 
 */
-(NSNumber*) getRandomWordsWithCompletionBlock :(NSString*) hasDictionaryDef 
     includePartOfSpeech:(NSString*) includePartOfSpeech 
     excludePartOfSpeech:(NSString*) excludePartOfSpeech 
     minCorpusCount:(NSNumber*) minCorpusCount 
     maxCorpusCount:(NSNumber*) maxCorpusCount 
     minDictionaryCount:(NSNumber*) minDictionaryCount 
     maxDictionaryCount:(NSNumber*) maxDictionaryCount 
     minLength:(NSNumber*) minLength 
     maxLength:(NSNumber*) maxLength 
     sortBy:(NSString*) sortBy 
     sortOrder:(NSString*) sortOrder 
     limit:(NSNumber*) limit 
    
    
    completionHandler: (void (^)(NSError* error))completionBlock;


/**

 Reverse dictionary search
 

 
 @param query Search term
 
 @param findSenseForWord Restricts words and finds closest sense
 
 @param includeSourceDictionaries Only include these comma-delimited source dictionaries
 
 @param excludeSourceDictionaries Exclude these comma-delimited source dictionaries
 
 @param includePartOfSpeech Only include these comma-delimited parts of speech
 
 @param excludePartOfSpeech Exclude these comma-delimited parts of speech
 
 @param minCorpusCount Minimum corpus frequency for terms
 
 @param maxCorpusCount Maximum corpus frequency for terms
 
 @param minLength Minimum word length
 
 @param maxLength Maximum word length
 
 @param expandTerms Expand terms
 
 @param includeTags Return a closed set of XML tags in response
 
 @param sortBy Attribute to sort by
 
 @param sortOrder Sort direction
 
 @param skip Results to skip
 
 @param limit Maximum number of results to return
 

 return type: SWGDefinitionSearchResults*
 */
-(NSNumber*) reverseDictionaryWithCompletionBlock :(NSString*) query 
     findSenseForWord:(NSString*) findSenseForWord 
     includeSourceDictionaries:(NSString*) includeSourceDictionaries 
     excludeSourceDictionaries:(NSString*) excludeSourceDictionaries 
     includePartOfSpeech:(NSString*) includePartOfSpeech 
     excludePartOfSpeech:(NSString*) excludePartOfSpeech 
     minCorpusCount:(NSNumber*) minCorpusCount 
     maxCorpusCount:(NSNumber*) maxCorpusCount 
     minLength:(NSNumber*) minLength 
     maxLength:(NSNumber*) maxLength 
     expandTerms:(NSString*) expandTerms 
     includeTags:(NSString*) includeTags 
     sortBy:(NSString*) sortBy 
     sortOrder:(NSString*) sortOrder 
     skip:(NSString*) skip 
     limit:(NSNumber*) limit 
    
    completionHandler: (void (^)(SWGDefinitionSearchResults* output, NSError* error))completionBlock;
    


/**

 Searches words
 

 
 @param query Search query
 
 @param caseSensitive Search case sensitive
 
 @param includePartOfSpeech Only include these comma-delimited parts of speech
 
 @param excludePartOfSpeech Exclude these comma-delimited parts of speech
 
 @param minCorpusCount Minimum corpus frequency for terms
 
 @param maxCorpusCount Maximum corpus frequency for terms
 
 @param minDictionaryCount Minimum number of dictionary entries for words returned
 
 @param maxDictionaryCount Maximum dictionary definition count
 
 @param minLength Minimum word length
 
 @param maxLength Maximum word length
 
 @param skip Results to skip
 
 @param limit Maximum number of results to return
 

 return type: SWGWordSearchResults*
 */
-(NSNumber*) searchWordsWithCompletionBlock :(NSString*) query 
     caseSensitive:(NSString*) caseSensitive 
     includePartOfSpeech:(NSString*) includePartOfSpeech 
     excludePartOfSpeech:(NSString*) excludePartOfSpeech 
     minCorpusCount:(NSNumber*) minCorpusCount 
     maxCorpusCount:(NSNumber*) maxCorpusCount 
     minDictionaryCount:(NSNumber*) minDictionaryCount 
     maxDictionaryCount:(NSNumber*) maxDictionaryCount 
     minLength:(NSNumber*) minLength 
     maxLength:(NSNumber*) maxLength 
     skip:(NSNumber*) skip 
     limit:(NSNumber*) limit 
    
    completionHandler: (void (^)(SWGWordSearchResults* output, NSError* error))completionBlock;
    


/**

 Returns a specific WordOfTheDay
 

 
 @param date Fetches by date in yyyy-MM-dd
 

 return type: SWGWordOfTheDay*
 */
-(NSNumber*) getWordOfTheDayWithCompletionBlock :(NSString*) date 
    
    completionHandler: (void (^)(SWGWordOfTheDay* output, NSError* error))completionBlock;
    



@end