#import "NIKDate.h"
#import "RVBPet.h"

@implementation RVBPet

-(id)name: (NSString*) name
    _id: (NSNumber*) _id
    tags: (NSArray*) tags
    status: (NSString*) status
    photoUrls: (NSArray*) photoUrls
    category: (RVBCategory*) category
{
  _name = name;
  __id = _id;
  _tags = tags;
  _status = status;
  _photoUrls = photoUrls;
  _category = category;
  return self;
}

-(id) initWithValues:(NSDictionary*)dict
{
    self = [super init];
    if(self) {
        _name = dict[@"name"]; 
        __id = dict[@"id"]; 
        id tags_dict = dict[@"tags"];
        if([tags_dict isKindOfClass:[NSArray class]]) {

            NSMutableArray * objs = [[NSMutableArray alloc] initWithCapacity:[(NSArray*)tags_dict count]];

            if([(NSArray*)tags_dict count] > 0) {
                for (NSDictionary* dict in (NSArray*)tags_dict) {
                    RVBTag* d = [[RVBTag alloc] initWithValues:dict];
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
        _photoUrls = dict[@"photoUrls"]; 
        id category_dict = dict[@"category"];
        _category = [[RVBCategory alloc]initWithValues:category_dict];
        

    }
    return self;
}

-(NSDictionary*) asDictionary {
    NSMutableDictionary* dict = [[NSMutableDictionary alloc] init];
    if(_name != nil) dict[@"name"] = _name ;
    if(__id != nil) dict[@"id"] = __id ;
    if(_tags != nil){
        if([_tags isKindOfClass:[NSArray class]]){
            NSMutableArray * array = [[NSMutableArray alloc] init];
            for( RVBTag *tags in (NSArray*)_tags) {
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
    if(_status != nil) dict[@"status"] = _status ;
    if(_photoUrls != nil) dict[@"photoUrls"] = _photoUrls ;
    if(_category != nil){
        if([_category isKindOfClass:[NSArray class]]){
            NSMutableArray * array = [[NSMutableArray alloc] init];
            for( RVBCategory *category in (NSArray*)_category) {
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
    NSDictionary* output = [dict copy];
    return output;
}

@end

