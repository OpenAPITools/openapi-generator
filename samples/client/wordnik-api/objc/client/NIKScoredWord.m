#import "NIKDate.h"
#import "NIKScoredWord.h"

@implementation NIKScoredWord

@synthesize position = _position;
@synthesize _id = __id;
@synthesize docTermCount = _docTermCount;
@synthesize lemma = _lemma;
@synthesize wordType = _wordType;
@synthesize score = _score;
@synthesize sentenceId = _sentenceId;
@synthesize word = _word;
@synthesize stopword = _stopword;
@synthesize baseWordScore = _baseWordScore;
@synthesize partOfSpeech = _partOfSpeech;
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
       partOfSpeech: (NSString*) partOfSpeech
       {
          _position = position;
          __id = _id;
          _docTermCount = docTermCount;
          _lemma = lemma;
          _wordType = wordType;
          _score = score;
          _sentenceId = sentenceId;
          _word = word;
          _stopword = stopword;
          _baseWordScore = baseWordScore;
          _partOfSpeech = partOfSpeech;
          return self;
       }

- (id) initWithValues: (NSDictionary*)dict
{
    _position = [dict objectForKey:@"position"];
    __id = [dict objectForKey:@"id"];
    _docTermCount = [dict objectForKey:@"docTermCount"];
    _lemma = [dict objectForKey:@"lemma"];
    _wordType = [dict objectForKey:@"wordType"];
    _score = [dict objectForKey:@"score"];
    _sentenceId = [dict objectForKey:@"sentenceId"];
    _word = [dict objectForKey:@"word"];
    _stopword = [dict objectForKey:@"stopword"];
    _baseWordScore = [dict objectForKey:@"baseWordScore"];
    _partOfSpeech = [dict objectForKey:@"partOfSpeech"];
    return self;
}

-(NSDictionary*) asDictionary {
    NSMutableDictionary* dict = [[NSMutableDictionary alloc] init];
    if(_position != nil) [dict setObject:_position forKey:@"position"];
    if(__id != nil) [dict setObject:__id forKey:@"id"];
    if(_docTermCount != nil) [dict setObject:_docTermCount forKey:@"docTermCount"];
    if(_lemma != nil) [dict setObject:_lemma forKey:@"lemma"];
    if(_wordType != nil) [dict setObject:_wordType forKey:@"wordType"];
    if(_score != nil) [dict setObject:_score forKey:@"score"];
    if(_sentenceId != nil) [dict setObject:_sentenceId forKey:@"sentenceId"];
    if(_word != nil) [dict setObject:_word forKey:@"word"];
    if(_stopword != nil) [dict setObject:_stopword forKey:@"stopword"];
    if(_baseWordScore != nil) [dict setObject:_baseWordScore forKey:@"baseWordScore"];
    if(_partOfSpeech != nil) [dict setObject:_partOfSpeech forKey:@"partOfSpeech"];
    NSDictionary* output = [dict copy];
    return output;
}

@end

