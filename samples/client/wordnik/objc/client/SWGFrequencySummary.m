#import "SWGDate.h"
#import "SWGFrequencySummary.h"

@implementation SWGFrequencySummary

-(id)unknownYearCount: (NSNumber*) unknownYearCount
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

-(id) initWithValues:(NSDictionary*)dict
{
    self = [super init];
    if(self) {
        _unknownYearCount = dict[@"unknownYearCount"];
        
        _totalCount = dict[@"totalCount"];
        
        _frequencyString = dict[@"frequencyString"];
        
        _word = dict[@"word"];
        
        
        
        id frequency_dict = dict[@"frequency"];
        
        if([frequency_dict isKindOfClass:[NSArray class]]) {
            NSMutableArray * objs = [[NSMutableArray alloc] initWithCapacity:[(NSArray*)frequency_dict count]];
            if([(NSArray*)frequency_dict count] > 0) {
                for (NSDictionary* dict in (NSArray*)frequency_dict) {
                    SWGFrequency* d = [[SWGFrequency alloc] initWithValues:dict];
                    [objs addObject:d];
                }
                _frequency = [[NSArray alloc] initWithArray:objs];
            }
            else
                _frequency = [[NSArray alloc] init];
        }
        else {
            _frequency = [[NSArray alloc] init];
        }
        
        
        
    }
    return self;
}

-(NSDictionary*) asDictionary {
    NSMutableDictionary* dict = [[NSMutableDictionary alloc] init];
    
    
            if(_unknownYearCount != nil) dict[@"unknownYearCount"] = _unknownYearCount ;
        
    
    
            if(_totalCount != nil) dict[@"totalCount"] = _totalCount ;
        
    
    
            if(_frequencyString != nil) dict[@"frequencyString"] = _frequencyString ;
        
    
    
            if(_word != nil) dict[@"word"] = _word ;
        
    
    
    if(_frequency != nil){
        if([_frequency isKindOfClass:[NSArray class]]){
            NSMutableArray * array = [[NSMutableArray alloc] init];
            for( SWGFrequency *frequency in (NSArray*)_frequency) {
                [array addObject:[(SWGObject*)frequency asDictionary]];
            }
            dict[@"frequency"] = array;
        }
        else if(_frequency && [_frequency isKindOfClass:[SWGDate class]]) {
            NSString * dateString = [(SWGDate*)_frequency toString];
            if(dateString){
                dict[@"frequency"] = dateString;
            }
        }
        else {
        
            if(_frequency != nil) dict[@"frequency"] = [(SWGObject*)_frequency asDictionary];
        
        }
    }
    
    

    NSDictionary* output = [dict copy];
    return output;
}

@end
