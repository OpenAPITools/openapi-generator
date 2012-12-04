#import "NIKDate.h"
#import "NIKSentence.h"

@implementation NIKSentence

@synthesize hasScoredWords = _hasScoredWords;
@synthesize _id = __id;
@synthesize scoredWords = _scoredWords;
@synthesize display = _display;
@synthesize rating = _rating;
@synthesize documentMetadataId = _documentMetadataId;
- (id) hasScoredWords: (NSNumber*) hasScoredWords
       _id: (NSNumber*) _id
       scoredWords: (NSArray*) scoredWords
       display: (NSString*) display
       rating: (NSNumber*) rating
       documentMetadataId: (NSNumber*) documentMetadataId
       {
          _hasScoredWords = hasScoredWords;
          __id = _id;
          _scoredWords = scoredWords;
          _display = display;
          _rating = rating;
          _documentMetadataId = documentMetadataId;
          return self;
       }

- (id) initWithValues: (NSDictionary*)dict
{
    _hasScoredWords = [dict objectForKey:@"hasScoredWords"];
    __id = [dict objectForKey:@"id"];
    id scoredWords_dict = [dict objectForKey:@"scoredWords"];
    if([scoredWords_dict isKindOfClass:[NSArray class]]) {
        if([(NSArray*)scoredWords_dict count] > 0) {
            NSMutableArray * objs = [[NSMutableArray alloc] initWithCapacity:[(NSArray*)scoredWords_dict count]];
            for (NSDictionary* dict in (NSArray*)scoredWords_dict) {
                NIKScoredWord* d = [[NIKScoredWord alloc]initWithValues:dict];
                [objs addObject:d];
            }
            _scoredWords = [[NSArray alloc] initWithArray:objs];
        }
    }
    _display = [dict objectForKey:@"display"];
    _rating = [dict objectForKey:@"rating"];
    _documentMetadataId = [dict objectForKey:@"documentMetadataId"];
    return self;
}

-(NSDictionary*) asDictionary {
    NSMutableDictionary* dict = [[NSMutableDictionary alloc] init];
    if(_hasScoredWords != nil) [dict setObject:_hasScoredWords forKey:@"hasScoredWords"];
    if(__id != nil) [dict setObject:__id forKey:@"id"];
    if(_scoredWords != nil){
        if([_scoredWords isKindOfClass:[NSArray class]]){
            NSMutableArray * array = [[NSMutableArray alloc] init];
            for( NIKScoredWord * scoredWords in (NSArray*)_scoredWords) {
                [array addObject:[(NIKSwaggerObject*)scoredWords asDictionary]];
            }
            [dict setObject:array forKey:@"scoredWords"];
        }
        else if(_scoredWords && [_scoredWords isKindOfClass:[NIKDate class]]) {
            NSString * dateString = [(NIKDate*)_scoredWords toString];
            if(dateString){
                [dict setObject:dateString forKey:@"scoredWords"];   
            }
        }
    }
    else {
    if(_scoredWords != nil) [dict setObject:[(NIKSwaggerObject*)_scoredWords asDictionary]forKey:@"scoredWords"];
    }
    if(_display != nil) [dict setObject:_display forKey:@"display"];
    if(_rating != nil) [dict setObject:_rating forKey:@"rating"];
    if(_documentMetadataId != nil) [dict setObject:_documentMetadataId forKey:@"documentMetadataId"];
    NSDictionary* output = [dict copy];
    return output;
}

@end

