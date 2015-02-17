#import "SWGDate.h"
#import "SWGPartOfSpeech.h"

@implementation SWGPartOfSpeech

-(id)roots: (NSArray*) roots
    storageAbbr: (NSArray*) storageAbbr
    allCategories: (NSArray*) allCategories
    
{
    _roots = roots;
    _storageAbbr = storageAbbr;
    _allCategories = allCategories;
    

    return self;
}

-(id) initWithValues:(NSDictionary*)dict
{
    self = [super init];
    if(self) {
        
        
        id roots_dict = dict[@"roots"];
        
        if([roots_dict isKindOfClass:[NSArray class]]) {
            NSMutableArray * objs = [[NSMutableArray alloc] initWithCapacity:[(NSArray*)roots_dict count]];
            if([(NSArray*)roots_dict count] > 0) {
                for (NSDictionary* dict in (NSArray*)roots_dict) {
                    SWGRoot* d = [[SWGRoot alloc] initWithValues:dict];
                    [objs addObject:d];
                }
                _roots = [[NSArray alloc] initWithArray:objs];
            }
            else
                _roots = [[NSArray alloc] init];
        }
        else {
            _roots = [[NSArray alloc] init];
        }
        
        
        _storageAbbr = dict[@"storageAbbr"];
        
        
        
        id allCategories_dict = dict[@"allCategories"];
        
        if([allCategories_dict isKindOfClass:[NSArray class]]) {
            NSMutableArray * objs = [[NSMutableArray alloc] initWithCapacity:[(NSArray*)allCategories_dict count]];
            if([(NSArray*)allCategories_dict count] > 0) {
                for (NSDictionary* dict in (NSArray*)allCategories_dict) {
                    SWGCategory* d = [[SWGCategory alloc] initWithValues:dict];
                    [objs addObject:d];
                }
                _allCategories = [[NSArray alloc] initWithArray:objs];
            }
            else
                _allCategories = [[NSArray alloc] init];
        }
        else {
            _allCategories = [[NSArray alloc] init];
        }
        
        
        
    }
    return self;
}

-(NSDictionary*) asDictionary {
    NSMutableDictionary* dict = [[NSMutableDictionary alloc] init];
    
    
    if(_roots != nil){
        if([_roots isKindOfClass:[NSArray class]]){
            NSMutableArray * array = [[NSMutableArray alloc] init];
            for( SWGRoot *roots in (NSArray*)_roots) {
                [array addObject:[(SWGObject*)roots asDictionary]];
            }
            dict[@"roots"] = array;
        }
        else if(_roots && [_roots isKindOfClass:[SWGDate class]]) {
            NSString * dateString = [(SWGDate*)_roots toString];
            if(dateString){
                dict[@"roots"] = dateString;
            }
        }
        else {
        
            if(_roots != nil) dict[@"roots"] = [(SWGObject*)_roots asDictionary];
        
        }
    }
    
    
    
            if(_storageAbbr != nil) dict[@"storageAbbr"] = _storageAbbr ;
        
    
    
    if(_allCategories != nil){
        if([_allCategories isKindOfClass:[NSArray class]]){
            NSMutableArray * array = [[NSMutableArray alloc] init];
            for( SWGCategory *allCategories in (NSArray*)_allCategories) {
                [array addObject:[(SWGObject*)allCategories asDictionary]];
            }
            dict[@"allCategories"] = array;
        }
        else if(_allCategories && [_allCategories isKindOfClass:[SWGDate class]]) {
            NSString * dateString = [(SWGDate*)_allCategories toString];
            if(dateString){
                dict[@"allCategories"] = dateString;
            }
        }
        else {
        
            if(_allCategories != nil) dict[@"allCategories"] = [(SWGObject*)_allCategories asDictionary];
        
        }
    }
    
    

    NSDictionary* output = [dict copy];
    return output;
}

@end
