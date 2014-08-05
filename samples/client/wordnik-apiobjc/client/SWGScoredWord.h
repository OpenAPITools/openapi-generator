#import <Foundation/Foundation.h>
#import "SWGObject.h"


@interface SWGScoredWord : SWGObject

@property(nonatomic) NSNumber* position;  

@property(nonatomic) NSNumber* _id;  

@property(nonatomic) NSNumber* docTermCount;  

@property(nonatomic) NSString* lemma;  

@property(nonatomic) NSString* wordType;  

@property(nonatomic) NSNumber* score;  

@property(nonatomic) NSNumber* sentenceId;  

@property(nonatomic) NSString* word;  

@property(nonatomic) NSNumber* stopword;  

@property(nonatomic) NSNumber* baseWordScore;  

@property(nonatomic) NSString* partOfSpeech;  

- (id) position: (NSNumber*) position
     _id: (NSNumber*) _id
     docTermCount: (NSNumber*) docTermCount
     lemma: (NSString*) lemma
     wordType: (NSString*) wordType
     score: (NSNumber*) score
     sentenceId: (NSNumber*) sentenceId
     word: (NSString*) word
     stopword: (NSNumber*) stopword
     baseWordScore: (NSNumber*) baseWordScore
     partOfSpeech: (NSString*) partOfSpeech;

- (id) initWithValues: (NSDictionary*)dict;
- (NSDictionary*) asDictionary;


@end

