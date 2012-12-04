#import <Foundation/Foundation.h>
#import "NIKSwaggerObject.h"

@interface NIKScoredWord : NIKSwaggerObject {
@private
    NSNumber* _position; //NSNumber
    NSNumber* __id; //NSNumber
    NSNumber* _docTermCount; //NSNumber
    NSString* _lemma; //NSString
    NSString* _wordType; //NSString
    NSNumber* _score; //NSNumber
    NSNumber* _sentenceId; //NSNumber
    NSString* _word; //NSString
    NSNumber* _stopword; //NSNumber
    NSNumber* _baseWordScore; //NSNumber
    NSString* _partOfSpeech; //NSString
    }



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

