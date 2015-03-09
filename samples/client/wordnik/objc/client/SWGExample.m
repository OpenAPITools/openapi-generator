#import "SWGDate.h"
#import "SWGExample.h"

@implementation SWGExample

-(id)_id: (NSNumber*) _id
    exampleId: (NSNumber*) exampleId
    title: (NSString*) title
    text: (NSString*) text
    score: (SWGScoredWord*) score
    sentence: (SWGSentence*) sentence
    word: (NSString*) word
    provider: (SWGContentProvider*) provider
    year: (NSNumber*) year
    rating: (NSNumber*) rating
    documentId: (NSNumber*) documentId
    url: (NSString*) url
    
{
    __id = _id;
    _exampleId = exampleId;
    _title = title;
    _text = text;
    _score = score;
    _sentence = sentence;
    _word = word;
    _provider = provider;
    _year = year;
    _rating = rating;
    _documentId = documentId;
    _url = url;
    

    return self;
}

-(id) initWithValues:(NSDictionary*)dict
{
    self = [super init];
    if(self) {
        __id = dict[@"id"];
        
        _exampleId = dict[@"exampleId"];
        
        _title = dict[@"title"];
        
        _text = dict[@"text"];
        
        
        
        id score_dict = dict[@"score"];
        
        if(score_dict != nil)
            _score = [[SWGScoredWord  alloc]initWithValues:score_dict];
        
        
        
        
        id sentence_dict = dict[@"sentence"];
        
        if(sentence_dict != nil)
            _sentence = [[SWGSentence  alloc]initWithValues:sentence_dict];
        
        
        _word = dict[@"word"];
        
        
        
        id provider_dict = dict[@"provider"];
        
        if(provider_dict != nil)
            _provider = [[SWGContentProvider  alloc]initWithValues:provider_dict];
        
        
        _year = dict[@"year"];
        
        _rating = dict[@"rating"];
        
        _documentId = dict[@"documentId"];
        
        _url = dict[@"url"];
        
        
    }
    return self;
}

-(NSDictionary*) asDictionary {
    NSMutableDictionary* dict = [[NSMutableDictionary alloc] init];
    
    
            if(__id != nil) dict[@"id"] = __id ;
        
    
    
            if(_exampleId != nil) dict[@"exampleId"] = _exampleId ;
        
    
    
            if(_title != nil) dict[@"title"] = _title ;
        
    
    
            if(_text != nil) dict[@"text"] = _text ;
        
    
    
    if(_score != nil){
        if([_score isKindOfClass:[NSArray class]]){
            NSMutableArray * array = [[NSMutableArray alloc] init];
            for( SWGScoredWord *score in (NSArray*)_score) {
                [array addObject:[(SWGObject*)score asDictionary]];
            }
            dict[@"score"] = array;
        }
        else if(_score && [_score isKindOfClass:[SWGDate class]]) {
            NSString * dateString = [(SWGDate*)_score toString];
            if(dateString){
                dict[@"score"] = dateString;
            }
        }
        else {
        
            if(_score != nil) dict[@"score"] = [(SWGObject*)_score asDictionary];
        
        }
    }
    
    
    
    if(_sentence != nil){
        if([_sentence isKindOfClass:[NSArray class]]){
            NSMutableArray * array = [[NSMutableArray alloc] init];
            for( SWGSentence *sentence in (NSArray*)_sentence) {
                [array addObject:[(SWGObject*)sentence asDictionary]];
            }
            dict[@"sentence"] = array;
        }
        else if(_sentence && [_sentence isKindOfClass:[SWGDate class]]) {
            NSString * dateString = [(SWGDate*)_sentence toString];
            if(dateString){
                dict[@"sentence"] = dateString;
            }
        }
        else {
        
            if(_sentence != nil) dict[@"sentence"] = [(SWGObject*)_sentence asDictionary];
        
        }
    }
    
    
    
            if(_word != nil) dict[@"word"] = _word ;
        
    
    
    if(_provider != nil){
        if([_provider isKindOfClass:[NSArray class]]){
            NSMutableArray * array = [[NSMutableArray alloc] init];
            for( SWGContentProvider *provider in (NSArray*)_provider) {
                [array addObject:[(SWGObject*)provider asDictionary]];
            }
            dict[@"provider"] = array;
        }
        else if(_provider && [_provider isKindOfClass:[SWGDate class]]) {
            NSString * dateString = [(SWGDate*)_provider toString];
            if(dateString){
                dict[@"provider"] = dateString;
            }
        }
        else {
        
            if(_provider != nil) dict[@"provider"] = [(SWGObject*)_provider asDictionary];
        
        }
    }
    
    
    
            if(_year != nil) dict[@"year"] = _year ;
        
    
    
            if(_rating != nil) dict[@"rating"] = _rating ;
        
    
    
            if(_documentId != nil) dict[@"documentId"] = _documentId ;
        
    
    
            if(_url != nil) dict[@"url"] = _url ;
        
    

    NSDictionary* output = [dict copy];
    return output;
}

@end
