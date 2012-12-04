#import "NIKDate.h"
#import "NIKFrequencySummary.h"

@implementation NIKFrequencySummary

@synthesize unknownYearCount = _unknownYearCount;
@synthesize totalCount = _totalCount;
@synthesize frequencyString = _frequencyString;
@synthesize word = _word;
@synthesize frequency = _frequency;
- (id) unknownYearCount: (NSNumber*) unknownYearCount
       totalCount: (NSNumber*) totalCount
       frequencyString: (NSString*) frequencyString
       word: (NSString*) word
       frequency: (NSArray*) frequency
       {
          _unknownYearCount = unknownYearCount;
          _totalCount = totalCount;
          _frequencyString = frequencyString;
          _word = word;
          _frequency = frequency;
          return self;
       }

- (id) initWithValues: (NSDictionary*)dict
{
    _unknownYearCount = [dict objectForKey:@"unknownYearCount"];
    _totalCount = [dict objectForKey:@"totalCount"];
    _frequencyString = [dict objectForKey:@"frequencyString"];
    _word = [dict objectForKey:@"word"];
    id frequency_dict = [dict objectForKey:@"frequency"];
    if([frequency_dict isKindOfClass:[NSArray class]]) {
        if([(NSArray*)frequency_dict count] > 0) {
            NSMutableArray * objs = [[NSMutableArray alloc] initWithCapacity:[(NSArray*)frequency_dict count]];
            for (NSDictionary* dict in (NSArray*)frequency_dict) {
                NIKFrequency* d = [[NIKFrequency alloc]initWithValues:dict];
                [objs addObject:d];
            }
            _frequency = [[NSArray alloc] initWithArray:objs];
        }
    }
    return self;
}

-(NSDictionary*) asDictionary {
    NSMutableDictionary* dict = [[NSMutableDictionary alloc] init];
    if(_unknownYearCount != nil) [dict setObject:_unknownYearCount forKey:@"unknownYearCount"];
    if(_totalCount != nil) [dict setObject:_totalCount forKey:@"totalCount"];
    if(_frequencyString != nil) [dict setObject:_frequencyString forKey:@"frequencyString"];
    if(_word != nil) [dict setObject:_word forKey:@"word"];
    if(_frequency != nil){
        if([_frequency isKindOfClass:[NSArray class]]){
            NSMutableArray * array = [[NSMutableArray alloc] init];
            for( NIKFrequency * frequency in (NSArray*)_frequency) {
                [array addObject:[(NIKSwaggerObject*)frequency asDictionary]];
            }
            [dict setObject:array forKey:@"frequency"];
        }
        else if(_frequency && [_frequency isKindOfClass:[NIKDate class]]) {
            NSString * dateString = [(NIKDate*)_frequency toString];
            if(dateString){
                [dict setObject:dateString forKey:@"frequency"];   
            }
        }
    }
    else {
    if(_frequency != nil) [dict setObject:[(NIKSwaggerObject*)_frequency asDictionary]forKey:@"frequency"];
    }
    NSDictionary* output = [dict copy];
    return output;
}

@end

