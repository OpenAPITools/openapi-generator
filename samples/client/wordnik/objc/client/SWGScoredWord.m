#import "SWGDate.h"
#import "SWGScoredWord.h"

@implementation SWGScoredWord

-(id)position: (NSNumber*) position
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

-(id) initWithValues:(NSDictionary*)dict
{
    self = [super init];
    if(self) {
        _position = dict[@"position"];
        
        __id = dict[@"id"];
        
        _docTermCount = dict[@"docTermCount"];
        
        _lemma = dict[@"lemma"];
        
        _wordType = dict[@"wordType"];
        
        _score = dict[@"score"];
        
        _sentenceId = dict[@"sentenceId"];
        
        _word = dict[@"word"];
        
        _stopword = dict[@"stopword"];
        
        _baseWordScore = dict[@"baseWordScore"];
        
        _partOfSpeech = dict[@"partOfSpeech"];
        
        
    }
    return self;
}

-(NSDictionary*) asDictionary {
    NSMutableDictionary* dict = [[NSMutableDictionary alloc] init];
    
    
            if(_position != nil) dict[@"position"] = _position ;
        
    
    
            if(__id != nil) dict[@"id"] = __id ;
        
    
    
            if(_docTermCount != nil) dict[@"docTermCount"] = _docTermCount ;
        
    
    
            if(_lemma != nil) dict[@"lemma"] = _lemma ;
        
    
    
            if(_wordType != nil) dict[@"wordType"] = _wordType ;
        
    
    
            if(_score != nil) dict[@"score"] = _score ;
        
    
    
            if(_sentenceId != nil) dict[@"sentenceId"] = _sentenceId ;
        
    
    
            if(_word != nil) dict[@"word"] = _word ;
        
    
    
            if(_stopword != nil) dict[@"stopword"] = _stopword ;
        
    
    
            if(_baseWordScore != nil) dict[@"baseWordScore"] = _baseWordScore ;
        
    
    
            if(_partOfSpeech != nil) dict[@"partOfSpeech"] = _partOfSpeech ;
        
    

    NSDictionary* output = [dict copy];
    return output;
}

@end
