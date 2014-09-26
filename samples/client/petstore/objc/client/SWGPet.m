#import "SWGDate.h"
#import "SWGPet.h"

@implementation SWGPet

-(id)_id: (NSNumber*) _id
    category: (SWGCategory*) category
    name: (NSString*) name
    photoUrls: (NSArray*) photoUrls
    tags: (NSArray*) tags
    status: (NSString*) status { 
    
    __id = _id;
    _category = category;
    _name = name;
    _photoUrls = photoUrls;
    _tags = tags;
    _status = status;
    
    return self;
}

-(id) initWithValues:(NSDictionary*)dict
{
    self = [super init];
    if(self) {
        __id = dict[@"id"];
        
        _name = dict[@"name"];
        
        
        
        id tags_dict = dict[@"tags"];
        if([tags_dict isKindOfClass:[NSArray class]]) {
            NSMutableArray * objs = [[NSMutableArray alloc] initWithCapacity:[(NSArray*)tags_dict count]];

            if([(NSArray*)tags_dict count] > 0) {
                for (NSDictionary* dict in (NSArray*)tags_dict) {
                    SWGTag* d = [[SWGTag  alloc] initWithValues:dict];
                    [objs addObject:d];
                }
                
                _tags = [[NSArray alloc] initWithArray:objs];
            }
            else {
                _tags = [[NSArray alloc] init];
            }
        }
        else {
            _tags = [[NSArray alloc] init];
        }
        
        _status = dict[@"status"];
        
    }
    return self;
}

-(NSDictionary*) asDictionary {
    NSMutableDictionary* dict = [[NSMutableDictionary alloc] init];
    if(__id != nil)
        dict[@"id"] = [(SWGObject*)__id asDictionary];
    if(_name != nil)
        dict[@"name"] = [(SWGObject*)_name asDictionary];
    if(_tags != nil){
        if([_tags isKindOfClass:[NSArray class]]){
            NSMutableArray * array = [[NSMutableArray alloc] init];
            for( SWGTag *tags in (NSArray*)_tags) {
                [array addObject:[(SWGObject*)tags asDictionary]];
            }
            dict[@"tags"] = array;
        }
        else if(_tags && [_tags isKindOfClass:[SWGDate class]]) {
            NSString * dateString = [(SWGDate*)_tags toString];
            if(dateString){
                dict[@"tags"] = dateString;
            }
        }
        else {
            if(_tags != nil) dict[@"tags"] = [(SWGObject*)_tags asDictionary];
        }
    }
    if(_status != nil)
        dict[@"status"] = [(SWGObject*)_status asDictionary];
    
    NSDictionary* output = [dict copy];
    return output;
}

@end
