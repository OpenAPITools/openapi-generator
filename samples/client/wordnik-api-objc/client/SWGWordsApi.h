#import <Foundation/Foundation.h>
#import "SWGDefinitionSearchResults.h"
#import "SWGWordObject.h"
#import "SWGWordOfTheDay.h"
#import "SWGWordSearchResults.h"



@interface SWGWordsApi: NSObject

-(void) addHeader:(NSString*)value forKey:(NSString*)key;
-(unsigned long) requestQueueSize;
+(SWGWordsApi*) apiWithHeader:(NSString*)headerValue key:(NSString*)key;
+(void) setBasePath:(NSString*)basePath;
+(NSString*) getBasePath;
/**

 Searches words
 
 @param query Search query
 @param includePartOfSpeech Only include these comma-delimited parts of speech
 @param excludePartOfSpeech Exclude these comma-delimited parts of speech
 @param caseSensitive Search case sensitive
 @param minCorpusCount Minimum corpus frequency for terms
 @param maxCorpusCount Maximum corpus frequency for terms
 @param minDictionaryCount Minimum number of dictionary entries for words returned
 @param maxDictionaryCount Maximum dictionary definition count
 @param minLength Minimum word length
 @param maxLength Maximum word length
 @param skip Results to skip
 @param limit Maximum number of results to return
 */
-(NSNumber*) searchWordsWithCompletionBlock :(NSString*) query 
        includePartOfSpeech:(NSString*) includePartOfSpeech 
        excludePartOfSpeech:(NSString*) excludePartOfSpeech 
        caseSensitive:(NSString*) caseSensitive 
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
 */
-(NSNumber*) getWordOfTheDayWithCompletionBlock :(NSString*) date 
        completionHandler: (void (^)(SWGWordOfTheDay* output, NSError* error))completionBlock;

/**

 Reverse dictionary search
 
 @param query Search term
 @param findSenseForWord Restricts words and finds closest sense
 @param includeSourceDictionaries Only include these comma-delimited source dictionaries
 @param excludeSourceDictionaries Exclude these comma-delimited source dictionaries
 @param includePartOfSpeech Only include these comma-delimited parts of speech
 @param excludePartOfSpeech Exclude these comma-delimited parts of speech
 @param expandTerms Expand terms
 @param sortBy Attribute to sort by
 @param sortOrder Sort direction
 @param minCorpusCount Minimum corpus frequency for terms
 @param maxCorpusCount Maximum corpus frequency for terms
 @param minLength Minimum word length
 @param maxLength Maximum word length
 @param includeTags Return a closed set of XML tags in response
 @param skip Results to skip
 @param limit Maximum number of results to return
 */
-(NSNumber*) reverseDictionaryWithCompletionBlock :(NSString*) query 
        findSenseForWord:(NSString*) findSenseForWord 
        includeSourceDictionaries:(NSString*) includeSourceDictionaries 
        excludeSourceDictionaries:(NSString*) excludeSourceDictionaries 
        includePartOfSpeech:(NSString*) includePartOfSpeech 
        excludePartOfSpeech:(NSString*) excludePartOfSpeech 
        expandTerms:(NSString*) expandTerms 
        sortBy:(NSString*) sortBy 
        sortOrder:(NSString*) sortOrder 
        minCorpusCount:(NSNumber*) minCorpusCount 
        maxCorpusCount:(NSNumber*) maxCorpusCount 
        minLength:(NSNumber*) minLength 
        maxLength:(NSNumber*) maxLength 
        includeTags:(NSString*) includeTags 
        skip:(NSString*) skip 
        limit:(NSNumber*) limit 
        completionHandler: (void (^)(SWGDefinitionSearchResults* output, NSError* error))completionBlock;

/**

 Returns an array of random WordObjects
 
 @param includePartOfSpeech CSV part-of-speech values to include
 @param excludePartOfSpeech CSV part-of-speech values to exclude
 @param sortBy Attribute to sort by
 @param sortOrder Sort direction
 @param hasDictionaryDef Only return words with dictionary definitions
 @param minCorpusCount Minimum corpus frequency for terms
 @param maxCorpusCount Maximum corpus frequency for terms
 @param minDictionaryCount Minimum dictionary count
 @param maxDictionaryCount Maximum dictionary count
 @param minLength Minimum word length
 @param maxLength Maximum word length
 @param limit Maximum number of results to return
 */
-(NSNumber*) getRandomWordsWithCompletionBlock :(NSString*) includePartOfSpeech 
        excludePartOfSpeech:(NSString*) excludePartOfSpeech 
        sortBy:(NSString*) sortBy 
        sortOrder:(NSString*) sortOrder 
        hasDictionaryDef:(NSString*) hasDictionaryDef 
        minCorpusCount:(NSNumber*) minCorpusCount 
        maxCorpusCount:(NSNumber*) maxCorpusCount 
        minDictionaryCount:(NSNumber*) minDictionaryCount 
        maxDictionaryCount:(NSNumber*) maxDictionaryCount 
        minLength:(NSNumber*) minLength 
        maxLength:(NSNumber*) maxLength 
        limit:(NSNumber*) limit 
        completionHandler: (void (^)(NSArray* output, NSError* error))completionBlock;

/**

 Returns a single random WordObject
 
 @param includePartOfSpeech CSV part-of-speech values to include
 @param excludePartOfSpeech CSV part-of-speech values to exclude
 @param hasDictionaryDef Only return words with dictionary definitions
 @param minCorpusCount Minimum corpus frequency for terms
 @param maxCorpusCount Maximum corpus frequency for terms
 @param minDictionaryCount Minimum dictionary count
 @param maxDictionaryCount Maximum dictionary count
 @param minLength Minimum word length
 @param maxLength Maximum word length
 */
-(NSNumber*) getRandomWordWithCompletionBlock :(NSString*) includePartOfSpeech 
        excludePartOfSpeech:(NSString*) excludePartOfSpeech 
        hasDictionaryDef:(NSString*) hasDictionaryDef 
        minCorpusCount:(NSNumber*) minCorpusCount 
        maxCorpusCount:(NSNumber*) maxCorpusCount 
        minDictionaryCount:(NSNumber*) minDictionaryCount 
        maxDictionaryCount:(NSNumber*) maxDictionaryCount 
        minLength:(NSNumber*) minLength 
        maxLength:(NSNumber*) maxLength 
        completionHandler: (void (^)(SWGWordObject* output, NSError* error))completionBlock;

@end
