#import "SWGDate.h"
#import "SWGRoot.h"

@implementation SWGRoot

-(id)_id: (NSNumber*) _id
    name: (NSString*) name
    categories: (NSArray*) categories
    
{
    __id = _id;
    _name = name;
    _categories = categories;
    

    return self;
}

-(id) initWithValues:(NSDictionary*)dict
{
    self = [super init];
    if(self) {
        __id = dict[@"id"];
        
        _name = dict[@"name"];
        
        
        
        id categories_dict = dict[@"categories"];
        
        if([categories_dict isKindOfClass:[NSArray class]]) {
            NSMutableArray * objs = [[NSMutableArray alloc] initWithCapacity:[(NSArray*)categories_dict count]];
            if([(NSArray*)categories_dict count] > 0) {
                for (NSDictionary* dict in (NSArray*)categories_dict) {
                    SWGCategory* d = [[SWGCategory alloc] initWithValues:dict];
                    [objs addObject:d];
                }
                _categories = [[NSArray alloc] initWithArray:objs];
            }
            else
                _categories = [[NSArray alloc] init];
        }
        else {
            _categories = [[NSArray alloc] init];
        }
        
        
        
    }
    return self;
}

-(NSDictionary*) asDictionary {
    NSMutableDictionary* dict = [[NSMutableDictionary alloc] init];
    
    
            if(__id != nil) dict[@"id"] = __id ;
        
    
    
            if(_name != nil) dict[@"name"] = _name ;
        
    
    
    if(_categories != nil){
        if([_categories isKindOfClass:[NSArray class]]){
            NSMutableArray * array = [[NSMutableArray alloc] init];
            for( SWGCategory *categories in (NSArray*)_categories) {
                [array addObject:[(SWGObject*)categories asDictionary]];
            }
            dict[@"categories"] = array;
        }
        else if(_categories && [_categories isKindOfClass:[SWGDate class]]) {
            NSString * dateString = [(SWGDate*)_categories toString];
            if(dateString){
                dict[@"categories"] = dateString;
            }
        }
        else {
        
            if(_categories != nil) dict[@"categories"] = [(SWGObject*)_categories asDictionary];
        
        }
    }
    
    

    NSDictionary* output = [dict copy];
    return output;
}

@end
