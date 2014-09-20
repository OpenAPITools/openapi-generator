#import "SWGDate.h"
#import "SWGSentence.h"

@implementation SWGSentence

-(id)hasScoredWords: (NSNumber*) hasScoredWords
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

-(id) initWithValues:(NSDictionary*)dict
{
    self = [super init];
    if(self) {
        _hasScoredWords = dict[@"hasScoredWords"]; 
        __id = dict[@"id"]; 
        id scoredWords_dict = dict[@"scoredWords"];
        if([scoredWords_dict isKindOfClass:[NSArray class]]) {

            NSMutableArray * objs = [[NSMutableArray alloc] initWithCapacity:[(NSArray*)scoredWords_dict count]];

            if([(NSArray*)scoredWords_dict count] > 0) {
                for (NSDictionary* dict in (NSArray*)scoredWords_dict) {
                    SWGScoredWord* d = [[SWGScoredWord alloc] initWithValues:dict];
                    [objs addObject:d];
                }
                
                _scoredWords = [[NSArray alloc] initWithArray:objs];
            }
            else {
                _scoredWords = [[NSArray alloc] init];
            }
        }
        else {
            _scoredWords = [[NSArray alloc] init];
        }
        _display = dict[@"display"]; 
        _rating = dict[@"rating"]; 
        _documentMetadataId = dict[@"documentMetadataId"]; 
        

    }
    return self;
}

-(NSDictionary*) asDictionary {
    NSMutableDictionary* dict = [[NSMutableDictionary alloc] init];
    if(_hasScoredWords != nil) dict[@"hasScoredWords"] = _hasScoredWords ;
        if(__id != nil) dict[@"id"] = __id ;
        if(_scoredWords != nil){
        if([_scoredWords isKindOfClass:[NSArray class]]){
            NSMutableArray * array = [[NSMutableArray alloc] init];
            for( SWGScoredWord *scoredWords in (NSArray*)_scoredWords) {
                [array addObject:[(SWGObject*)scoredWords asDictionary]];
            }
            dict[@"scoredWords"] = array;
        }
        else if(_scoredWords && [_scoredWords isKindOfClass:[SWGDate class]]) {
            NSString * dateString = [(SWGDate*)_scoredWords toString];
            if(dateString){
                dict[@"scoredWords"] = dateString;
            }
        }
        else {
        if(_scoredWords != nil) dict[@"scoredWords"] = [(SWGObject*)_scoredWords asDictionary];
        }
    }
    if(_display != nil) dict[@"display"] = _display ;
        if(_rating != nil) dict[@"rating"] = _rating ;
        if(_documentMetadataId != nil) dict[@"documentMetadataId"] = _documentMetadataId ;
        NSDictionary* output = [dict copy];
    return output;
}

@end

