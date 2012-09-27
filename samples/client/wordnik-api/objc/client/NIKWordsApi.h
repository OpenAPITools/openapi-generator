#import <Foundation/Foundation.h>
#import "NIKApiInvoker.h"
#import "NIKWordObject.h"
#import "NIKDefinitionSearchResults.h"
#import "NIKWordOfTheDay.h"
#import "NIKWordSearchResults.h"


@interface NIKWordsApi: NSObject {

@private
    NSOperationQueue *_queue;
    NIKApiInvoker * _api;
}
@property(nonatomic, readonly) NSOperationQueue* queue;
@property(nonatomic, readonly) NIKApiInvoker* api;

-(void) addHeader:(NSString*) value
           forKey:(NSString*)key;

-(void) searchWordsWithCompletionBlock :(NSString*) query includePartOfSpeech:(NSString*) includePartOfSpeech excludePartOfSpeech:(NSString*) excludePartOfSpeech caseSensitive:(NSString*) caseSensitive minCorpusCount:(NSNumber*) minCorpusCount maxCorpusCount:(NSNumber*) maxCorpusCount minDictionaryCount:(NSNumber*) minDictionaryCount maxDictionaryCount:(NSNumber*) maxDictionaryCount minLength:(NSNumber*) minLength maxLength:(NSNumber*) maxLength skip:(NSNumber*) skip limit:(NSNumber*) limit 
        completionHandler:(void (^)(NIKWordSearchResults*, NSError *))completionBlock;
-(void) getWordOfTheDayWithCompletionBlock :(NSString*) date 
        completionHandler:(void (^)(NIKWordOfTheDay*, NSError *))completionBlock;
-(void) reverseDictionaryWithCompletionBlock :(NSString*) query findSenseForWord:(NSString*) findSenseForWord includeSourceDictionaries:(NSString*) includeSourceDictionaries excludeSourceDictionaries:(NSString*) excludeSourceDictionaries includePartOfSpeech:(NSString*) includePartOfSpeech excludePartOfSpeech:(NSString*) excludePartOfSpeech expandTerms:(NSString*) expandTerms sortBy:(NSString*) sortBy sortOrder:(NSString*) sortOrder minCorpusCount:(NSNumber*) minCorpusCount maxCorpusCount:(NSNumber*) maxCorpusCount minLength:(NSNumber*) minLength maxLength:(NSNumber*) maxLength includeTags:(NSString*) includeTags skip:(NSString*) skip limit:(NSNumber*) limit 
        completionHandler:(void (^)(NIKDefinitionSearchResults*, NSError *))completionBlock;
-(void) getRandomWordsWithCompletionBlock :(NSString*) includePartOfSpeech excludePartOfSpeech:(NSString*) excludePartOfSpeech sortBy:(NSString*) sortBy sortOrder:(NSString*) sortOrder hasDictionaryDef:(NSString*) hasDictionaryDef minCorpusCount:(NSNumber*) minCorpusCount maxCorpusCount:(NSNumber*) maxCorpusCount minDictionaryCount:(NSNumber*) minDictionaryCount maxDictionaryCount:(NSNumber*) maxDictionaryCount minLength:(NSNumber*) minLength maxLength:(NSNumber*) maxLength limit:(NSNumber*) limit 
        completionHandler:(void (^)(NSArray*, NSError *))completionBlock;
-(void) getRandomWordWithCompletionBlock :(NSString*) includePartOfSpeech excludePartOfSpeech:(NSString*) excludePartOfSpeech hasDictionaryDef:(NSString*) hasDictionaryDef minCorpusCount:(NSNumber*) minCorpusCount maxCorpusCount:(NSNumber*) maxCorpusCount minDictionaryCount:(NSNumber*) minDictionaryCount maxDictionaryCount:(NSNumber*) maxDictionaryCount minLength:(NSNumber*) minLength maxLength:(NSNumber*) maxLength 
        completionHandler:(void (^)(NIKWordObject*, NSError *))completionBlock;
@end
