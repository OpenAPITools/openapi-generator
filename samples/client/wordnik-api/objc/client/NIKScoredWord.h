#import <Foundation/Foundation.h>
#import "NIKSwaggerObject.h"

@interface NIKScoredWord : NIKSwaggerObject {
@private
    NSNumber* __id; //NSNumber
    NSNumber* _position; //NSNumber
    NSString* _lemma; //NSString
    NSNumber* _docTermCount; //NSNumber
    NSString* _wordType; //NSString
    NSNumber* _score; //NSNumber
    NSString* _word; //NSString
    NSNumber* _sentenceId; //NSNumber
    NSNumber* _stopword; //NSNumber
    NSNumber* _baseWordScore; //NSNumber
    NSString* _partOfSpeech; //NSString
    }



@property(nonatomic) NSNumber* _id;
@property(nonatomic) NSNumber* position;
@property(nonatomic) NSString* lemma;
@property(nonatomic) NSNumber* docTermCount;
@property(nonatomic) NSString* wordType;
@property(nonatomic) NSNumber* score;
@property(nonatomic) NSString* word;
@property(nonatomic) NSNumber* sentenceId;
@property(nonatomic) NSNumber* stopword;
@property(nonatomic) NSNumber* baseWordScore;
@property(nonatomic) NSString* partOfSpeech;
- (id) _id: (NSNumber*) _id
     position: (NSNumber*) position
     lemma: (NSString*) lemma
     docTermCount: (NSNumber*) docTermCount
     wordType: (NSString*) wordType
     score: (NSNumber*) score
     word: (NSString*) word
     sentenceId: (NSNumber*) sentenceId
     stopword: (NSNumber*) stopword
     baseWordScore: (NSNumber*) baseWordScore
     partOfSpeech: (NSString*) partOfSpeech;

- (id) initWithValues: (NSDictionary*)dict;
- (NSDictionary*) asDictionary;


@end

