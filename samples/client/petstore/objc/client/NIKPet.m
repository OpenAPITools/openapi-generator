#import "NIKDate.h"
#import "NIKPet.h"

@implementation NIKPet

@synthesize _id = __id;
@synthesize tags = _tags;
@synthesize category = _category;
@synthesize status = _status;
@synthesize name = _name;
@synthesize photoUrls = _photoUrls;
- (id) _id: (NSNumber*) _id
       tags: (NSArray*) tags
       category: (NIKCategory*) category
       status: (NSString*) status
       name: (NSString*) name
       photoUrls: (NSArray*) photoUrls
       {
          __id = _id;
          _tags = tags;
          _category = category;
          _status = status;
          _name = name;
          _photoUrls = photoUrls;
          return self;
       }

- (id) initWithValues: (NSDictionary*)dict
{
    __id = [dict objectForKey:@"id"];
    id tags_dict = [dict objectForKey:@"tags"];
    if([tags_dict isKindOfClass:[NSArray class]]) {
        if([(NSArray*)tags_dict count] > 0) {
            NSMutableArray * objs = [[NSMutableArray alloc] initWithCapacity:[(NSArray*)tags_dict count]];
            for (NSDictionary* dict in (NSArray*)tags_dict) {
                NIKTag* d = [[NIKTag alloc]initWithValues:dict];
                [objs addObject:d];
            }
            _tags = [[NSArray alloc] initWithArray:objs];
        }
    }
    id category_dict = [dict objectForKey:@"category"];
    _category = [[NIKCategory alloc]initWithValues:category_dict];
    _status = [dict objectForKey:@"status"];
    _name = [dict objectForKey:@"name"];
    _photoUrls = [dict objectForKey:@"photoUrls"];
    return self;
}

-(NSDictionary*) asDictionary {
    NSMutableDictionary* dict = [[NSMutableDictionary alloc] init];
    if(__id != nil) [dict setObject:__id forKey:@"id"];
    if(_tags != nil){
        if([_tags isKindOfClass:[NSArray class]]){
            NSMutableArray * array = [[NSMutableArray alloc] init];
            for( NIKTag * tags in (NSArray*)_tags) {
                [array addObject:[(NIKSwaggerObject*)tags asDictionary]];
            }
            [dict setObject:array forKey:@"tags"];
        }
        else if(_tags && [_tags isKindOfClass:[NIKDate class]]) {
            NSString * dateString = [(NIKDate*)_tags toString];
            if(dateString){
                [dict setObject:dateString forKey:@"tags"];   
            }
        }
    }
    else {
    if(_tags != nil) [dict setObject:[(NIKSwaggerObject*)_tags asDictionary]forKey:@"tags"];
    }
    if(_category != nil){
        if([_category isKindOfClass:[NSArray class]]){
            NSMutableArray * array = [[NSMutableArray alloc] init];
            for( NIKCategory * category in (NSArray*)_category) {
                [array addObject:[(NIKSwaggerObject*)category asDictionary]];
            }
            [dict setObject:array forKey:@"category"];
        }
        else if(_category && [_category isKindOfClass:[NIKDate class]]) {
            NSString * dateString = [(NIKDate*)_category toString];
            if(dateString){
                [dict setObject:dateString forKey:@"category"];   
            }
        }
    }
    else {
    if(_category != nil) [dict setObject:[(NIKSwaggerObject*)_category asDictionary]forKey:@"category"];
    }
    if(_status != nil) [dict setObject:_status forKey:@"status"];
    if(_name != nil) [dict setObject:_name forKey:@"name"];
    if(_photoUrls != nil) [dict setObject:_photoUrls forKey:@"photoUrls"];
    NSDictionary* output = [dict copy];
    return output;
}

@end

