#import "NIKExample.h"

@implementation NIKExample

@synthesize _id = __id;
@synthesize text = _text;
@synthesize title = _title;
@synthesize exampleId = _exampleId;
@synthesize score = _score;
@synthesize sentence = _sentence;
@synthesize year = _year;
@synthesize provider = _provider;
@synthesize word = _word;
@synthesize rating = _rating;
@synthesize url = _url;
@synthesize documentId = _documentId;
- (id) _id: (NSNumber*) _id
       text: (NSString*) text
       title: (NSString*) title
       exampleId: (NSNumber*) exampleId
       score: (NIKScoredWord*) score
       sentence: (NIKSentence*) sentence
       year: (NSNumber*) year
       provider: (NIKContentProvider*) provider
       word: (NSString*) word
       rating: (NSNumber*) rating
       url: (NSString*) url
       documentId: (NSNumber*) documentId
       {
          __id = _id;
          _text = text;
          _title = title;
          _exampleId = exampleId;
          _score = score;
          _sentence = sentence;
          _year = year;
          _provider = provider;
          _word = word;
          _rating = rating;
          _url = url;
          _documentId = documentId;
          return self;
       }

- (id) initWithValues: (NSDictionary*)dict
{
    __id = [dict objectForKey:@"id"];
    _text = [dict objectForKey:@"text"];
    _title = [dict objectForKey:@"title"];
    _exampleId = [dict objectForKey:@"exampleId"];
    id score_dict = [dict objectForKey:@"score"];
    _score = [[NIKScoredWord alloc]initWithValues:score_dict];
    id sentence_dict = [dict objectForKey:@"sentence"];
    _sentence = [[NIKSentence alloc]initWithValues:sentence_dict];
    _year = [dict objectForKey:@"year"];
    id provider_dict = [dict objectForKey:@"provider"];
    _provider = [[NIKContentProvider alloc]initWithValues:provider_dict];
    _word = [dict objectForKey:@"word"];
    _rating = [dict objectForKey:@"rating"];
    _url = [dict objectForKey:@"url"];
    _documentId = [dict objectForKey:@"documentId"];
    return self;
}

-(NSDictionary*) asDictionary {
    NSMutableDictionary* dict = [[NSMutableDictionary alloc] init];
    if(__id != nil) [dict setObject:__id forKey:@"id"];
    if(_text != nil) [dict setObject:_text forKey:@"text"];
    if(_title != nil) [dict setObject:_title forKey:@"title"];
    if(_exampleId != nil) [dict setObject:_exampleId forKey:@"exampleId"];
    if(_score != nil){
        if([_score isKindOfClass:[NSArray class]]){
            NSMutableArray * array = [[NSMutableArray alloc] init];
            for( NIKScoredWord * score in (NSArray*)_score) {
                [array addObject:[(NIKSwaggerObject*)score asDictionary]];
            }
            [dict setObject:array forKey:@"score"];
        }
        else if(_score && [_score isKindOfClass:[NIKDate class]]) {
            NSString * dateString = [(NIKDate*)_score toString];
            if(dateString){
                [dict setObject:dateString forKey:@"score"];   
            }
        }
    }
    else {
    if(_score != nil) [dict setObject:[(NIKSwaggerObject*)_score asDictionary]forKey:@"score"];
    }
    if(_sentence != nil){
        if([_sentence isKindOfClass:[NSArray class]]){
            NSMutableArray * array = [[NSMutableArray alloc] init];
            for( NIKSentence * sentence in (NSArray*)_sentence) {
                [array addObject:[(NIKSwaggerObject*)sentence asDictionary]];
            }
            [dict setObject:array forKey:@"sentence"];
        }
        else if(_sentence && [_sentence isKindOfClass:[NIKDate class]]) {
            NSString * dateString = [(NIKDate*)_sentence toString];
            if(dateString){
                [dict setObject:dateString forKey:@"sentence"];   
            }
        }
    }
    else {
    if(_sentence != nil) [dict setObject:[(NIKSwaggerObject*)_sentence asDictionary]forKey:@"sentence"];
    }
    if(_year != nil) [dict setObject:_year forKey:@"year"];
    if(_provider != nil){
        if([_provider isKindOfClass:[NSArray class]]){
            NSMutableArray * array = [[NSMutableArray alloc] init];
            for( NIKContentProvider * provider in (NSArray*)_provider) {
                [array addObject:[(NIKSwaggerObject*)provider asDictionary]];
            }
            [dict setObject:array forKey:@"provider"];
        }
        else if(_provider && [_provider isKindOfClass:[NIKDate class]]) {
            NSString * dateString = [(NIKDate*)_provider toString];
            if(dateString){
                [dict setObject:dateString forKey:@"provider"];   
            }
        }
    }
    else {
    if(_provider != nil) [dict setObject:[(NIKSwaggerObject*)_provider asDictionary]forKey:@"provider"];
    }
    if(_word != nil) [dict setObject:_word forKey:@"word"];
    if(_rating != nil) [dict setObject:_rating forKey:@"rating"];
    if(_url != nil) [dict setObject:_url forKey:@"url"];
    if(_documentId != nil) [dict setObject:_documentId forKey:@"documentId"];
    NSDictionary* output = [dict copy];
    return output;
}

@end

