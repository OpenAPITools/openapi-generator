#import "NIKDate.h"
#import "NIKWordListWord.h"

@implementation NIKWordListWord

@synthesize _id = __id;
@synthesize username = _username;
@synthesize createdAt = _createdAt;
@synthesize numberCommentsOnWord = _numberCommentsOnWord;
@synthesize userId = _userId;
@synthesize word = _word;
@synthesize numberLists = _numberLists;
- (id) _id: (NSNumber*) _id
       username: (NSString*) username
       createdAt: (NIKDate*) createdAt
       numberCommentsOnWord: (NSNumber*) numberCommentsOnWord
       userId: (NSNumber*) userId
       word: (NSString*) word
       numberLists: (NSNumber*) numberLists
       {
          __id = _id;
          _username = username;
          _createdAt = createdAt;
          _numberCommentsOnWord = numberCommentsOnWord;
          _userId = userId;
          _word = word;
          _numberLists = numberLists;
          return self;
       }

- (id) initWithValues: (NSDictionary*)dict
{
    __id = [dict objectForKey:@"id"];
    _username = [dict objectForKey:@"username"];
    id createdAt_dict = [dict objectForKey:@"createdAt"];
    _createdAt = [[NIKDate alloc]initWithValues:createdAt_dict];
    _numberCommentsOnWord = [dict objectForKey:@"numberCommentsOnWord"];
    _userId = [dict objectForKey:@"userId"];
    _word = [dict objectForKey:@"word"];
    _numberLists = [dict objectForKey:@"numberLists"];
    return self;
}

-(NSDictionary*) asDictionary {
    NSMutableDictionary* dict = [[NSMutableDictionary alloc] init];
    if(__id != nil) [dict setObject:__id forKey:@"id"];
    if(_username != nil) [dict setObject:_username forKey:@"username"];
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
    if(_numberCommentsOnWord != nil) [dict setObject:_numberCommentsOnWord forKey:@"numberCommentsOnWord"];
    if(_userId != nil) [dict setObject:_userId forKey:@"userId"];
    if(_word != nil) [dict setObject:_word forKey:@"word"];
    if(_numberLists != nil) [dict setObject:_numberLists forKey:@"numberLists"];
    NSDictionary* output = [dict copy];
    return output;
}

@end

