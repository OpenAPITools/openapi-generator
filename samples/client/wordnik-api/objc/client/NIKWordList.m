#import "NIKDate.h"
#import "NIKWordList.h"

@implementation NIKWordList

@synthesize updatedAt = _updatedAt;
@synthesize _id = __id;
@synthesize username = _username;
@synthesize permalink = _permalink;
@synthesize lastActivityAt = _lastActivityAt;
@synthesize createdAt = _createdAt;
@synthesize description = _description;
@synthesize userId = _userId;
@synthesize name = _name;
@synthesize numberWordsInList = _numberWordsInList;
@synthesize type = _type;
- (id) updatedAt: (NIKDate*) updatedAt
       _id: (NSNumber*) _id
       username: (NSString*) username
       permalink: (NSString*) permalink
       lastActivityAt: (NIKDate*) lastActivityAt
       createdAt: (NIKDate*) createdAt
       description: (NSString*) description
       userId: (NSNumber*) userId
       name: (NSString*) name
       numberWordsInList: (NSNumber*) numberWordsInList
       type: (NSString*) type
       {
          _updatedAt = updatedAt;
          __id = _id;
          _username = username;
          _permalink = permalink;
          _lastActivityAt = lastActivityAt;
          _createdAt = createdAt;
          _description = description;
          _userId = userId;
          _name = name;
          _numberWordsInList = numberWordsInList;
          _type = type;
          return self;
       }

- (id) initWithValues: (NSDictionary*)dict
{
    id updatedAt_dict = [dict objectForKey:@"updatedAt"];
    _updatedAt = [[NIKDate alloc]initWithValues:updatedAt_dict];
    __id = [dict objectForKey:@"id"];
    _username = [dict objectForKey:@"username"];
    _permalink = [dict objectForKey:@"permalink"];
    id lastActivityAt_dict = [dict objectForKey:@"lastActivityAt"];
    _lastActivityAt = [[NIKDate alloc]initWithValues:lastActivityAt_dict];
    id createdAt_dict = [dict objectForKey:@"createdAt"];
    _createdAt = [[NIKDate alloc]initWithValues:createdAt_dict];
    _description = [dict objectForKey:@"description"];
    _userId = [dict objectForKey:@"userId"];
    _name = [dict objectForKey:@"name"];
    _numberWordsInList = [dict objectForKey:@"numberWordsInList"];
    _type = [dict objectForKey:@"type"];
    return self;
}

-(NSDictionary*) asDictionary {
    NSMutableDictionary* dict = [[NSMutableDictionary alloc] init];
    if(_updatedAt != nil){
        if([_updatedAt isKindOfClass:[NSArray class]]){
            NSMutableArray * array = [[NSMutableArray alloc] init];
            for( NIKDate * updatedAt in (NSArray*)_updatedAt) {
                [array addObject:[(NIKSwaggerObject*)updatedAt asDictionary]];
            }
            [dict setObject:array forKey:@"updatedAt"];
        }
        else if(_updatedAt && [_updatedAt isKindOfClass:[NIKDate class]]) {
            NSString * dateString = [(NIKDate*)_updatedAt toString];
            if(dateString){
                [dict setObject:dateString forKey:@"updatedAt"];   
            }
        }
    }
    else {
    if(_updatedAt != nil) [dict setObject:[(NIKSwaggerObject*)_updatedAt asDictionary]forKey:@"updatedAt"];
    }
    if(__id != nil) [dict setObject:__id forKey:@"id"];
    if(_username != nil) [dict setObject:_username forKey:@"username"];
    if(_permalink != nil) [dict setObject:_permalink forKey:@"permalink"];
    if(_lastActivityAt != nil){
        if([_lastActivityAt isKindOfClass:[NSArray class]]){
            NSMutableArray * array = [[NSMutableArray alloc] init];
            for( NIKDate * lastActivityAt in (NSArray*)_lastActivityAt) {
                [array addObject:[(NIKSwaggerObject*)lastActivityAt asDictionary]];
            }
            [dict setObject:array forKey:@"lastActivityAt"];
        }
        else if(_lastActivityAt && [_lastActivityAt isKindOfClass:[NIKDate class]]) {
            NSString * dateString = [(NIKDate*)_lastActivityAt toString];
            if(dateString){
                [dict setObject:dateString forKey:@"lastActivityAt"];   
            }
        }
    }
    else {
    if(_lastActivityAt != nil) [dict setObject:[(NIKSwaggerObject*)_lastActivityAt asDictionary]forKey:@"lastActivityAt"];
    }
    if(_createdAt != nil){
        if([_createdAt isKindOfClass:[NSArray class]]){
            NSMutableArray * array = [[NSMutableArray alloc] init];
            for( NIKDate * createdAt in (NSArray*)_createdAt) {
                [array addObject:[(NIKSwaggerObject*)createdAt asDictionary]];
            }
            [dict setObject:array forKey:@"createdAt"];
        }
        else if(_createdAt && [_createdAt isKindOfClass:[NIKDate class]]) {
            NSString * dateString = [(NIKDate*)_createdAt toString];
            if(dateString){
                [dict setObject:dateString forKey:@"createdAt"];   
            }
        }
    }
    else {
    if(_createdAt != nil) [dict setObject:[(NIKSwaggerObject*)_createdAt asDictionary]forKey:@"createdAt"];
    }
    if(_description != nil) [dict setObject:_description forKey:@"description"];
    if(_userId != nil) [dict setObject:_userId forKey:@"userId"];
    if(_name != nil) [dict setObject:_name forKey:@"name"];
    if(_numberWordsInList != nil) [dict setObject:_numberWordsInList forKey:@"numberWordsInList"];
    if(_type != nil) [dict setObject:_type forKey:@"type"];
    NSDictionary* output = [dict copy];
    return output;
}

@end

