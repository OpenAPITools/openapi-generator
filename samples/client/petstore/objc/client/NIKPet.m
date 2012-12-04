#import "NIKDate.h"
#import "NIKPet.h"

@implementation NIKPet

-(id)tags: (NSArray*) tags
    _id: (NSNumber*) _id
    category: (NIKCategory*) category
    status: (NSString*) status
    name: (NSString*) name
    photoUrls: (NSArray*) photoUrls
{
  _tags = tags;
  __id = _id;
  _category = category;
  _status = status;
  _name = name;
  _photoUrls = photoUrls;
  return self;
}

-(id) initWithValues:(NSDictionary*)dict
{
    self = [super init];
    if(self) {
        id tags_dict = dict[@"tags"];
        if([tags_dict isKindOfClass:[NSArray class]]) {

            NSMutableArray * objs = [[NSMutableArray alloc] initWithCapacity:[(NSArray*)tags_dict count]];

            if([(NSArray*)tags_dict count] > 0) {
                for (NSDictionary* dict in (NSArray*)tags_dict) {
                    NIKTag* d = [[NIKTag alloc] initWithValues:dict];
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
        __id = dict[@"id"]; 
        id category_dict = dict[@"category"];
        _category = [[NIKCategory alloc]initWithValues:category_dict];
        _status = dict[@"status"]; 
        _name = dict[@"name"]; 
        _photoUrls = dict[@"photoUrls"]; 
        

    }
    return self;
}

-(NSDictionary*) asDictionary {
    NSMutableDictionary* dict = [[NSMutableDictionary alloc] init];
    if(_tags != nil){
        if([_tags isKindOfClass:[NSArray class]]){
            NSMutableArray * array = [[NSMutableArray alloc] init];
            for( NIKTag *tags in (NSArray*)_tags) {
                [array addObject:[(NIKSwaggerObject*)tags asDictionary]];
            }
            dict[@"tags"] = array;
        }
        else if(_tags && [_tags isKindOfClass:[NIKDate class]]) {
            NSString * dateString = [(NIKDate*)_tags toString];
            if(dateString){
                dict[@"tags"] = dateString;
            }
        }
    }
    else {
    if(_tags != nil) dict[@"tags"] = [(NIKSwaggerObject*)_tags asDictionary];
    }
    if(__id != nil) dict[@"id"] = __id ;
    if(_category != nil){
        if([_category isKindOfClass:[NSArray class]]){
            NSMutableArray * array = [[NSMutableArray alloc] init];
            for( NIKCategory *category in (NSArray*)_category) {
                [array addObject:[(NIKSwaggerObject*)category asDictionary]];
            }
            dict[@"category"] = array;
        }
        else if(_category && [_category isKindOfClass:[NIKDate class]]) {
            NSString * dateString = [(NIKDate*)_category toString];
            if(dateString){
                dict[@"category"] = dateString;
            }
        }
    }
    else {
    if(_category != nil) dict[@"category"] = [(NIKSwaggerObject*)_category asDictionary];
    }
    if(_status != nil) dict[@"status"] = _status ;
    if(_name != nil) dict[@"name"] = _name ;
    if(_photoUrls != nil) dict[@"photoUrls"] = _photoUrls ;
    NSDictionary* output = [dict copy];
    return output;
}

@end

