#import "SWGDate.h"
#import "SWGWordList.h"

@implementation SWGWordList

-(id)_id: (NSNumber*) _id
    permalink: (NSString*) permalink
    name: (NSString*) name
    createdAt: (SWGDate*) createdAt
    updatedAt: (SWGDate*) updatedAt
    lastActivityAt: (SWGDate*) lastActivityAt
    username: (NSString*) username
    userId: (NSNumber*) userId
    description: (NSString*) description
    numberWordsInList: (NSNumber*) numberWordsInList
    type: (NSString*) type
{
  __id = _id;
  _permalink = permalink;
  _name = name;
  _createdAt = createdAt;
  _updatedAt = updatedAt;
  _lastActivityAt = lastActivityAt;
  _username = username;
  _userId = userId;
  _description = description;
  _numberWordsInList = numberWordsInList;
  _type = type;
  return self;
}

-(id) initWithValues:(NSDictionary*)dict
{
    self = [super init];
    if(self) {
        __id = dict[@"id"]; 
        _permalink = dict[@"permalink"]; 
        _name = dict[@"name"]; 
        id createdAt_dict = dict[@"createdAt"];
        if(createdAt_dict != nil)
            _createdAt = [[SWGDate alloc]initWithValues:createdAt_dict];
        id updatedAt_dict = dict[@"updatedAt"];
        if(updatedAt_dict != nil)
            _updatedAt = [[SWGDate alloc]initWithValues:updatedAt_dict];
        id lastActivityAt_dict = dict[@"lastActivityAt"];
        if(lastActivityAt_dict != nil)
            _lastActivityAt = [[SWGDate alloc]initWithValues:lastActivityAt_dict];
        _username = dict[@"username"]; 
        _userId = dict[@"userId"]; 
        _description = dict[@"description"]; 
        _numberWordsInList = dict[@"numberWordsInList"]; 
        _type = dict[@"type"]; 
        

    }
    return self;
}

-(NSDictionary*) asDictionary {
    NSMutableDictionary* dict = [[NSMutableDictionary alloc] init];
    if(__id != nil) dict[@"id"] = __id ;
        if(_permalink != nil) dict[@"permalink"] = _permalink ;
        if(_name != nil) dict[@"name"] = _name ;
        if(_createdAt != nil){
        if([_createdAt isKindOfClass:[NSArray class]]){
            NSMutableArray * array = [[NSMutableArray alloc] init];
            for( SWGDate *createdAt in (NSArray*)_createdAt) {
                [array addObject:[(SWGObject*)createdAt asDictionary]];
            }
            dict[@"createdAt"] = array;
        }
        else if(_createdAt && [_createdAt isKindOfClass:[SWGDate class]]) {
            NSString * dateString = [(SWGDate*)_createdAt toString];
            if(dateString){
                dict[@"createdAt"] = dateString;
            }
        }
        else {
        if(_createdAt != nil) dict[@"createdAt"] = [(SWGObject*)_createdAt asDictionary];
        }
    }
    if(_updatedAt != nil){
        if([_updatedAt isKindOfClass:[NSArray class]]){
            NSMutableArray * array = [[NSMutableArray alloc] init];
            for( SWGDate *updatedAt in (NSArray*)_updatedAt) {
                [array addObject:[(SWGObject*)updatedAt asDictionary]];
            }
            dict[@"updatedAt"] = array;
        }
        else if(_updatedAt && [_updatedAt isKindOfClass:[SWGDate class]]) {
            NSString * dateString = [(SWGDate*)_updatedAt toString];
            if(dateString){
                dict[@"updatedAt"] = dateString;
            }
        }
        else {
        if(_updatedAt != nil) dict[@"updatedAt"] = [(SWGObject*)_updatedAt asDictionary];
        }
    }
    if(_lastActivityAt != nil){
        if([_lastActivityAt isKindOfClass:[NSArray class]]){
            NSMutableArray * array = [[NSMutableArray alloc] init];
            for( SWGDate *lastActivityAt in (NSArray*)_lastActivityAt) {
                [array addObject:[(SWGObject*)lastActivityAt asDictionary]];
            }
            dict[@"lastActivityAt"] = array;
        }
        else if(_lastActivityAt && [_lastActivityAt isKindOfClass:[SWGDate class]]) {
            NSString * dateString = [(SWGDate*)_lastActivityAt toString];
            if(dateString){
                dict[@"lastActivityAt"] = dateString;
            }
        }
        else {
        if(_lastActivityAt != nil) dict[@"lastActivityAt"] = [(SWGObject*)_lastActivityAt asDictionary];
        }
    }
    if(_username != nil) dict[@"username"] = _username ;
        if(_userId != nil) dict[@"userId"] = _userId ;
        if(_description != nil) dict[@"description"] = _description ;
        if(_numberWordsInList != nil) dict[@"numberWordsInList"] = _numberWordsInList ;
        if(_type != nil) dict[@"type"] = _type ;
        NSDictionary* output = [dict copy];
    return output;
}

@end

