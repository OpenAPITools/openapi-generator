#import <Foundation/Foundation.h>
#import "NIKApiInvoker.h"
#import "NIKDefinition.h"
#import "NIKTextPron.h"
#import "NIKExample.h"
#import "NIKSyllable.h"
#import "NIKAudioFile.h"
#import "NIKExampleSearchResults.h"
#import "NIKWordObject.h"
#import "NIKBigram.h"
#import "NIKRelated.h"
#import "NIKFrequencySummary.h"


@interface NIKWordApi: NSObject {

@private
    NSOperationQueue *_queue;
    NIKApiInvoker * _api;
}
@property(nonatomic, readonly) NSOperationQueue* queue;
@property(nonatomic, readonly) NIKApiInvoker* api;

-(void) addHeader:(NSString*) value
           forKey:(NSString*)key;

-(void) getExamplesWithCompletionBlock :(NSString*) word includeDuplicates:(NSString*) includeDuplicates useCanonical:(NSString*) useCanonical skip:(NSNumber*) skip limit:(NSNumber*) limit 
        completionHandler:(void (^)(NIKExampleSearchResults*, NSError *))completionBlock;
-(void) getWordWithCompletionBlock :(NSString*) word useCanonical:(NSString*) useCanonical includeSuggestions:(NSString*) includeSuggestions 
        completionHandler:(void (^)(NIKWordObject*, NSError *))completionBlock;
-(void) getDefinitionsWithCompletionBlock :(NSString*) word partOfSpeech:(NSString*) partOfSpeech sourceDictionaries:(NSString*) sourceDictionaries limit:(NSNumber*) limit includeRelated:(NSString*) includeRelated useCanonical:(NSString*) useCanonical includeTags:(NSString*) includeTags 
        completionHandler:(void (^)(NSArray*, NSError *))completionBlock;
-(void) getTopExampleWithCompletionBlock :(NSString*) word useCanonical:(NSString*) useCanonical 
        completionHandler:(void (^)(NIKExample*, NSError *))completionBlock;
-(void) getRelatedWordsWithCompletionBlock :(NSString*) word relationshipTypes:(NSString*) relationshipTypes useCanonical:(NSString*) useCanonical limitPerRelationshipType:(NSNumber*) limitPerRelationshipType 
        completionHandler:(void (^)(NSArray*, NSError *))completionBlock;
-(void) getTextPronunciationsWithCompletionBlock :(NSString*) word sourceDictionary:(NSString*) sourceDictionary typeFormat:(NSString*) typeFormat useCanonical:(NSString*) useCanonical limit:(NSNumber*) limit 
        completionHandler:(void (^)(NSArray*, NSError *))completionBlock;
-(void) getHyphenationWithCompletionBlock :(NSString*) word sourceDictionary:(NSString*) sourceDictionary useCanonical:(NSString*) useCanonical limit:(NSNumber*) limit 
        completionHandler:(void (^)(NSArray*, NSError *))completionBlock;
-(void) getWordFrequencyWithCompletionBlock :(NSString*) word useCanonical:(NSString*) useCanonical startYear:(NSNumber*) startYear endYear:(NSNumber*) endYear 
        completionHandler:(void (^)(NIKFrequencySummary*, NSError *))completionBlock;
-(void) getPhrasesWithCompletionBlock :(NSString*) word limit:(NSNumber*) limit wlmi:(NSNumber*) wlmi useCanonical:(NSString*) useCanonical 
        completionHandler:(void (^)(NSArray*, NSError *))completionBlock;
-(void) getEtymologiesWithCompletionBlock :(NSString*) word useCanonical:(NSString*) useCanonical 
        completionHandler:(void (^)(NSArray*, NSError *))completionBlock;
-(void) getAudioWithCompletionBlock :(NSString*) word useCanonical:(NSString*) useCanonical limit:(NSNumber*) limit 
        completionHandler:(void (^)(NSArray*, NSError *))completionBlock;
@end
