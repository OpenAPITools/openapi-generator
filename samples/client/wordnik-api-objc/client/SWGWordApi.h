#import <Foundation/Foundation.h>
#import "SWGFrequencySummary.h"
#import "SWGBigram.h"
#import "SWGWordObject.h"
#import "SWGExampleSearchResults.h"
#import "SWGExample.h"
#import "SWGScrabbleScoreResult.h"
#import "SWGTextPron.h"
#import "SWGSyllable.h"
#import "SWGRelated.h"
#import "SWGDefinition.h"
#import "SWGAudioFile.h"



@interface SWGWordApi: NSObject

-(void) addHeader:(NSString*)value forKey:(NSString*)key;
-(unsigned long) requestQueueSize;
+(SWGWordApi*) apiWithHeader:(NSString*)headerValue key:(NSString*)key;
+(void) setBasePath:(NSString*)basePath;
+(NSString*) getBasePath;
/**

 Returns examples for a word
 
 @param word Word to return examples for
 @param includeDuplicates Show duplicate examples from different sources
 @param useCanonical If true will try to return the correct word root ('cats' -&gt; 'cat'). If false returns exactly what was requested.
 @param skip Results to skip
 @param limit Maximum number of results to return
 */
-(NSNumber*) getExamplesWithCompletionBlock :(NSString*) word 
        includeDuplicates:(NSString*) includeDuplicates 
        useCanonical:(NSString*) useCanonical 
        skip:(NSNumber*) skip 
        limit:(NSNumber*) limit 
        completionHandler: (void (^)(SWGExampleSearchResults* output, NSError* error))completionBlock;

/**

 Given a word as a string, returns the WordObject that represents it
 
 @param word String value of WordObject to return
 @param useCanonical If true will try to return the correct word root ('cats' -&gt; 'cat'). If false returns exactly what was requested.
 @param includeSuggestions Return suggestions (for correct spelling, case variants, etc.)
 */
-(NSNumber*) getWordWithCompletionBlock :(NSString*) word 
        useCanonical:(NSString*) useCanonical 
        includeSuggestions:(NSString*) includeSuggestions 
        completionHandler: (void (^)(SWGWordObject* output, NSError* error))completionBlock;

/**

 Return definitions for a word
 
 @param word Word to return definitions for
 @param partOfSpeech CSV list of part-of-speech types
 @param sourceDictionaries Source dictionary to return definitions from.  If 'all' is received, results are returned from all sources. If multiple values are received (e.g. 'century,wiktionary'), results are returned from the first specified dictionary that has definitions. If left blank, results are returned from the first dictionary that has definitions. By default, dictionaries are searched in this order: ahd, wiktionary, webster, century, wordnet
 @param limit Maximum number of results to return
 @param includeRelated Return related words with definitions
 @param useCanonical If true will try to return the correct word root ('cats' -&gt; 'cat'). If false returns exactly what was requested.
 @param includeTags Return a closed set of XML tags in response
 */
-(NSNumber*) getDefinitionsWithCompletionBlock :(NSString*) word 
        partOfSpeech:(NSString*) partOfSpeech 
        sourceDictionaries:(NSString*) sourceDictionaries 
        limit:(NSNumber*) limit 
        includeRelated:(NSString*) includeRelated 
        useCanonical:(NSString*) useCanonical 
        includeTags:(NSString*) includeTags 
        completionHandler: (void (^)(NSArray* output, NSError* error))completionBlock;

/**

 Returns a top example for a word
 
 @param word Word to fetch examples for
 @param useCanonical If true will try to return the correct word root ('cats' -&gt; 'cat'). If false returns exactly what was requested.
 */
-(NSNumber*) getTopExampleWithCompletionBlock :(NSString*) word 
        useCanonical:(NSString*) useCanonical 
        completionHandler: (void (^)(SWGExample* output, NSError* error))completionBlock;

/**

 Given a word as a string, returns relationships from the Word Graph
 
 @param word Word to fetch relationships for
 @param relationshipTypes Limits the total results per type of relationship type
 @param useCanonical If true will try to return the correct word root ('cats' -&gt; 'cat'). If false returns exactly what was requested.
 @param limitPerRelationshipType Restrict to the supplied relatinship types
 */
-(NSNumber*) getRelatedWordsWithCompletionBlock :(NSString*) word 
        relationshipTypes:(NSString*) relationshipTypes 
        useCanonical:(NSString*) useCanonical 
        limitPerRelationshipType:(NSNumber*) limitPerRelationshipType 
        completionHandler: (void (^)(NSArray* output, NSError* error))completionBlock;

/**

 Returns text pronunciations for a given word
 
 @param word Word to get pronunciations for
 @param sourceDictionary Get from a single dictionary
 @param typeFormat Text pronunciation type
 @param useCanonical If true will try to return a correct word root ('cats' -&gt; 'cat'). If false returns exactly what was requested.
 @param limit Maximum number of results to return
 */
-(NSNumber*) getTextPronunciationsWithCompletionBlock :(NSString*) word 
        sourceDictionary:(NSString*) sourceDictionary 
        typeFormat:(NSString*) typeFormat 
        useCanonical:(NSString*) useCanonical 
        limit:(NSNumber*) limit 
        completionHandler: (void (^)(NSArray* output, NSError* error))completionBlock;

/**

 Returns syllable information for a word
 
 @param word Word to get syllables for
 @param sourceDictionary Get from a single dictionary. Valid options: ahd, century, wiktionary, webster, and wordnet.
 @param useCanonical If true will try to return a correct word root ('cats' -&gt; 'cat'). If false returns exactly what was requested.
 @param limit Maximum number of results to return
 */
-(NSNumber*) getHyphenationWithCompletionBlock :(NSString*) word 
        sourceDictionary:(NSString*) sourceDictionary 
        useCanonical:(NSString*) useCanonical 
        limit:(NSNumber*) limit 
        completionHandler: (void (^)(NSArray* output, NSError* error))completionBlock;

/**

 Returns word usage over time
 
 @param word Word to return
 @param useCanonical If true will try to return the correct word root ('cats' -&gt; 'cat'). If false returns exactly what was requested.
 @param startYear Starting Year
 @param endYear Ending Year
 */
-(NSNumber*) getWordFrequencyWithCompletionBlock :(NSString*) word 
        useCanonical:(NSString*) useCanonical 
        startYear:(NSNumber*) startYear 
        endYear:(NSNumber*) endYear 
        completionHandler: (void (^)(SWGFrequencySummary* output, NSError* error))completionBlock;

/**

 Fetches bi-gram phrases for a word
 
 @param word Word to fetch phrases for
 @param limit Maximum number of results to return
 @param wlmi Minimum WLMI for the phrase
 @param useCanonical If true will try to return the correct word root ('cats' -&gt; 'cat'). If false returns exactly what was requested.
 */
-(NSNumber*) getPhrasesWithCompletionBlock :(NSString*) word 
        limit:(NSNumber*) limit 
        wlmi:(NSNumber*) wlmi 
        useCanonical:(NSString*) useCanonical 
        completionHandler: (void (^)(NSArray* output, NSError* error))completionBlock;

/**

 Fetches etymology data
 
 @param word Word to return
 @param useCanonical If true will try to return the correct word root ('cats' -&gt; 'cat'). If false returns exactly what was requested.
 */
-(NSNumber*) getEtymologiesWithCompletionBlock :(NSString*) word 
        useCanonical:(NSString*) useCanonical 
        completionHandler: (void (^)(NSArray* output, NSError* error))completionBlock;

/**

 Fetches audio metadata for a word.
 The metadata includes a time-expiring fileUrl which allows reading the audio file directly from the API.  Currently only audio pronunciations from the American Heritage Dictionary in mp3 format are supported.
 @param word Word to get audio for.
 @param useCanonical Use the canonical form of the word
 @param limit Maximum number of results to return
 */
-(NSNumber*) getAudioWithCompletionBlock :(NSString*) word 
        useCanonical:(NSString*) useCanonical 
        limit:(NSNumber*) limit 
        completionHandler: (void (^)(NSArray* output, NSError* error))completionBlock;

/**

 Returns the Scrabble score for a word
 
 @param word Word to get scrabble score for.
 */
-(NSNumber*) getScrabbleScoreWithCompletionBlock :(NSString*) word 
        completionHandler: (void (^)(SWGScrabbleScoreResult* output, NSError* error))completionBlock;

@end
