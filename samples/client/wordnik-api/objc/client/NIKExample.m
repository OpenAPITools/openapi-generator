#import "NIKDate.h"
#import "NIKExample.h"

@implementation NIKExample

@synthesize _id = __id;
@synthesize exampleId = _exampleId;
@synthesize title = _title;
@synthesize text = _text;
@synthesize score = _score;
@synthesize sentence = _sentence;
@synthesize word = _word;
@synthesize provider = _provider;
@synthesize year = _year;
@synthesize rating = _rating;
@synthesize documentId = _documentId;
@synthesize url = _url;
- (id) _id: (NSNumber*) _id
       exampleId: (NSNumber*) exampleId
       title: (NSString*) title
       text: (NSString*) text
       score: (NIKScoredWord*) score
       sentence: (NIKSentence*) sentence
       word: (NSString*) word
       provider: (NIKContentProvider*) provider
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

- (id) initWithValues: (NSDictionary*)dict
{
    __id = [dict objectForKey:@"id"];
    _exampleId = [dict objectForKey:@"exampleId"];
    _title = [dict objectForKey:@"title"];
    _text = [dict objectForKey:@"text"];
    id score_dict = [dict objectForKey:@"score"];
    _score = [[NIKScoredWord alloc]initWithValues:score_dict];
    id sentence_dict = [dict objectForKey:@"sentence"];
    _sentence = [[NIKSentence alloc]initWithValues:sentence_dict];
    _word = [dict objectForKey:@"word"];
    id provider_dict = [dict objectForKey:@"provider"];
    _provider = [[NIKContentProvider alloc]initWithValues:provider_dict];
    _year = [dict objectForKey:@"year"];
    _rating = [dict objectForKey:@"rating"];
    _documentId = [dict objectForKey:@"documentId"];
    _url = [dict objectForKey:@"url"];
    return self;
}

-(NSDictionary*) asDictionary {
    NSMutableDictionary* dict = [[NSMutableDictionary alloc] init];
    if(__id != nil) [dict setObject:__id forKey:@"id"];
    if(_exampleId != nil) [dict setObject:_exampleId forKey:@"exampleId"];
    if(_title != nil) [dict setObject:_title forKey:@"title"];
    if(_text != nil) [dict setObject:_text forKey:@"text"];
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
    if(_word != nil) [dict setObject:_word forKey:@"word"];
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
    if(_year != nil) [dict setObject:_year forKey:@"year"];
    if(_rating != nil) [dict setObject:_rating forKey:@"rating"];
    if(_documentId != nil) [dict setObject:_documentId forKey:@"documentId"];
    if(_url != nil) [dict setObject:_url forKey:@"url"];
    NSDictionary* output = [dict copy];
    return output;
}

@end

