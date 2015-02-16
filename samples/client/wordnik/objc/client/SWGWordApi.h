#import <Foundation/Foundation.h>
#import "SWGWordObject.h"
#import "SWGAudioFile.h"
#import "SWGDefinition.h"
#import "SWGFrequencySummary.h"
#import "SWGBigram.h"
#import "SWGExample.h"
#import "SWGObject.h"


@interface SWGWordApi: NSObject

-(void) addHeader:(NSString*)value forKey:(NSString*)key;
-(unsigned long) requestQueueSize;
+(SWGWordApi*) apiWithHeader:(NSString*)headerValue key:(NSString*)key;
+(void) setBasePath:(NSString*)basePath;
+(NSString*) getBasePath;
/**

 Given a word as a string, returns the WordObject that represents it
 

 
 @param word String value of WordObject to return
 
 @param useCanonical If true will try to return the correct word root (&#39;cats&#39; -&gt; &#39;cat&#39;). If false returns exactly what was requested.
 
 @param includeSuggestions Return suggestions (for correct spelling, case variants, etc.)
 

 return type: SWGWordObject*
 */
-(NSNumber*) getWordWithCompletionBlock :(NSString*) word 
     useCanonical:(NSString*) useCanonical 
     includeSuggestions:(NSString*) includeSuggestions 
    
    completionHandler: (void (^)(SWGWordObject* output, NSError* error))completionBlock;
    


/**

 Fetches audio metadata for a word.
 
 The metadata includes a time-expiring fileUrl which allows reading the audio file directly from the API.  Currently only audio pronunciations from the American Heritage Dictionary in mp3 format are supported.
 

 
 @param word Word to get audio for.
 
 @param useCanonical Use the canonical form of the word
 
 @param limit Maximum number of results to return
 

 return type: NSArray*
 */
-(NSNumber*) getAudioWithCompletionBlock :(NSString*) word 
     useCanonical:(NSString*) useCanonical 
     limit:(NSNumber*) limit 
    
    completionHandler: (void (^)(NSArray* output, NSError* error))completionBlock;
    


/**

 Return definitions for a word
 

 
 @param word Word to return definitions for
 
 @param limit Maximum number of results to return
 
 @param partOfSpeech CSV list of part-of-speech types
 
 @param includeRelated Return related words with definitions
 
 @param sourceDictionaries Source dictionary to return definitions from.  If &#39;all&#39; is received, results are returned from all sources. If multiple values are received (e.g. &#39;century,wiktionary&#39;), results are returned from the first specified dictionary that has definitions. If left blank, results are returned from the first dictionary that has definitions. By default, dictionaries are searched in this order: ahd, wiktionary, webster, century, wordnet
 
 @param useCanonical If true will try to return the correct word root (&#39;cats&#39; -&gt; &#39;cat&#39;). If false returns exactly what was requested.
 
 @param includeTags Return a closed set of XML tags in response
 

 return type: NSArray*
 */
-(NSNumber*) getDefinitionsWithCompletionBlock :(NSString*) word 
     limit:(NSNumber*) limit 
     partOfSpeech:(NSString*) partOfSpeech 
     includeRelated:(NSString*) includeRelated 
     sourceDictionaries:(NSArray*) sourceDictionaries 
     useCanonical:(NSString*) useCanonical 
     includeTags:(NSString*) includeTags 
    
    completionHandler: (void (^)(NSArray* output, NSError* error))completionBlock;
    


/**

 Fetches etymology data
 

 
 @param word Word to return
 
 @param useCanonical If true will try to return the correct word root (&#39;cats&#39; -&gt; &#39;cat&#39;). If false returns exactly what was requested.
 

 return type: NSArray*
 */
-(NSNumber*) getEtymologiesWithCompletionBlock :(NSString*) word 
     useCanonical:(NSString*) useCanonical 
    
    completionHandler: (void (^)(NSArray* output, NSError* error))completionBlock;
    


/**

 Returns examples for a word
 

 
 @param word Word to return examples for
 
 @param includeDuplicates Show duplicate examples from different sources
 
 @param useCanonical If true will try to return the correct word root (&#39;cats&#39; -&gt; &#39;cat&#39;). If false returns exactly what was requested.
 
 @param skip Results to skip
 
 @param limit Maximum number of results to return
 

 return type: 
 */
-(NSNumber*) getExamplesWithCompletionBlock :(NSString*) word 
     includeDuplicates:(NSString*) includeDuplicates 
     useCanonical:(NSString*) useCanonical 
     skip:(NSNumber*) skip 
     limit:(NSNumber*) limit 
    
    
    completionHandler: (void (^)(NSError* error))completionBlock;


/**

 Returns word usage over time
 

 
 @param word Word to return
 
 @param useCanonical If true will try to return the correct word root (&#39;cats&#39; -&gt; &#39;cat&#39;). If false returns exactly what was requested.
 
 @param startYear Starting Year
 
 @param endYear Ending Year
 

 return type: SWGFrequencySummary*
 */
-(NSNumber*) getWordFrequencyWithCompletionBlock :(NSString*) word 
     useCanonical:(NSString*) useCanonical 
     startYear:(NSNumber*) startYear 
     endYear:(NSNumber*) endYear 
    
    completionHandler: (void (^)(SWGFrequencySummary* output, NSError* error))completionBlock;
    


/**

 Returns syllable information for a word
 

 
 @param word Word to get syllables for
 
 @param useCanonical If true will try to return a correct word root (&#39;cats&#39; -&gt; &#39;cat&#39;). If false returns exactly what was requested.
 
 @param sourceDictionary Get from a single dictionary. Valid options: ahd, century, wiktionary, webster, and wordnet.
 
 @param limit Maximum number of results to return
 

 return type: 
 */
-(NSNumber*) getHyphenationWithCompletionBlock :(NSString*) word 
     useCanonical:(NSString*) useCanonical 
     sourceDictionary:(NSString*) sourceDictionary 
     limit:(NSNumber*) limit 
    
    
    completionHandler: (void (^)(NSError* error))completionBlock;


/**

 Fetches bi-gram phrases for a word
 

 
 @param word Word to fetch phrases for
 
 @param limit Maximum number of results to return
 
 @param wlmi Minimum WLMI for the phrase
 
 @param useCanonical If true will try to return the correct word root (&#39;cats&#39; -&gt; &#39;cat&#39;). If false returns exactly what was requested.
 

 return type: NSArray*
 */
-(NSNumber*) getPhrasesWithCompletionBlock :(NSString*) word 
     limit:(NSNumber*) limit 
     wlmi:(NSNumber*) wlmi 
     useCanonical:(NSString*) useCanonical 
    
    completionHandler: (void (^)(NSArray* output, NSError* error))completionBlock;
    


/**

 Returns text pronunciations for a given word
 

 
 @param word Word to get pronunciations for
 
 @param useCanonical If true will try to return a correct word root (&#39;cats&#39; -&gt; &#39;cat&#39;). If false returns exactly what was requested.
 
 @param sourceDictionary Get from a single dictionary
 
 @param typeFormat Text pronunciation type
 
 @param limit Maximum number of results to return
 

 return type: 
 */
-(NSNumber*) getTextPronunciationsWithCompletionBlock :(NSString*) word 
     useCanonical:(NSString*) useCanonical 
     sourceDictionary:(NSString*) sourceDictionary 
     typeFormat:(NSString*) typeFormat 
     limit:(NSNumber*) limit 
    
    
    completionHandler: (void (^)(NSError* error))completionBlock;


/**

 Given a word as a string, returns relationships from the Word Graph
 

 
 @param word Word to fetch relationships for
 
 @param useCanonical If true will try to return the correct word root (&#39;cats&#39; -&gt; &#39;cat&#39;). If false returns exactly what was requested.
 
 @param relationshipTypes Limits the total results per type of relationship type
 
 @param limitPerRelationshipType Restrict to the supplied relationship types
 

 return type: 
 */
-(NSNumber*) getRelatedWordsWithCompletionBlock :(NSString*) word 
     useCanonical:(NSString*) useCanonical 
     relationshipTypes:(NSString*) relationshipTypes 
     limitPerRelationshipType:(NSNumber*) limitPerRelationshipType 
    
    
    completionHandler: (void (^)(NSError* error))completionBlock;


/**

 Returns a top example for a word
 

 
 @param word Word to fetch examples for
 
 @param useCanonical If true will try to return the correct word root (&#39;cats&#39; -&gt; &#39;cat&#39;). If false returns exactly what was requested.
 

 return type: SWGExample*
 */
-(NSNumber*) getTopExampleWithCompletionBlock :(NSString*) word 
     useCanonical:(NSString*) useCanonical 
    
    completionHandler: (void (^)(SWGExample* output, NSError* error))completionBlock;
    



@end