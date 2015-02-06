#import "SWGDate.h"
#import "SWGWordListWord.h"

@implementation SWGWordListWord

-(id)_id: (NSNumber*) _id
    word: (NSString*) word
    username: (NSString*) username
    userId: (NSNumber*) userId
    createdAt: (SWGDate*) createdAt
    numberCommentsOnWord: (NSNumber*) numberCommentsOnWord
    numberLists: (NSNumber*) numberLists
    
{
    __id = _id;
    _word = word;
    _username = username;
    _userId = userId;
    _createdAt = createdAt;
    _numberCommentsOnWord = numberCommentsOnWord;
    _numberLists = numberLists;
    

    return self;
}

-(id) initWithValues:(NSDictionary*)dict
{
    self = [super init];
    if(self) {
        __id = dict[@"id"];
        
        _word = dict[@"word"];
        
        _username = dict[@"username"];
        
        _userId = dict[@"userId"];
        
        
        
        id createdAt_dict = dict[@"createdAt"];
        
        if(createdAt_dict != nil)
            _createdAt = [[SWGDate  alloc]initWithValues:createdAt_dict];
        
        
        _numberCommentsOnWord = dict[@"numberCommentsOnWord"];
        
        _numberLists = dict[@"numberLists"];
        
        
    }
    return self;
}

-(NSDictionary*) asDictionary {
    NSMutableDictionary* dict = [[NSMutableDictionary alloc] init];
    
    
            if(__id != nil) dict[@"id"] = __id ;
        
    
    
            if(_word != nil) dict[@"word"] = _word ;
        
    
    
            if(_username != nil) dict[@"username"] = _username ;
        
    
    
            if(_userId != nil) dict[@"userId"] = _userId ;
        
    
    
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
    
    
    
            if(_numberCommentsOnWord != nil) dict[@"numberCommentsOnWord"] = _numberCommentsOnWord ;
        
    
    
            if(_numberLists != nil) dict[@"numberLists"] = _numberLists ;
        
    

    NSDictionary* output = [dict copy];
    return output;
}

@end
