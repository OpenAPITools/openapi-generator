#import "SWGDate.h"
#import "SWGWordOfTheDay.h"

@implementation SWGWordOfTheDay

-(id)_id: (NSNumber*) _id
    parentId: (NSString*) parentId
    category: (NSString*) category
    createdBy: (NSString*) createdBy
    createdAt: (SWGDate*) createdAt
    contentProvider: (SWGContentProvider*) contentProvider
    htmlExtra: (NSString*) htmlExtra
    word: (NSString*) word
    definitions: (NSArray*) definitions
    examples: (NSArray*) examples
    note: (NSString*) note
    publishDate: (SWGDate*) publishDate
{
  __id = _id;
  _parentId = parentId;
  _category = category;
  _createdBy = createdBy;
  _createdAt = createdAt;
  _contentProvider = contentProvider;
  _htmlExtra = htmlExtra;
  _word = word;
  _definitions = definitions;
  _examples = examples;
  _note = note;
  _publishDate = publishDate;
  return self;
}

-(id) initWithValues:(NSDictionary*)dict
{
    self = [super init];
    if(self) {
        __id = dict[@"id"]; 
        _parentId = dict[@"parentId"]; 
        _category = dict[@"category"]; 
        _createdBy = dict[@"createdBy"]; 
        id createdAt_dict = dict[@"createdAt"];
        if(createdAt_dict != nil)
            _createdAt = [[SWGDate alloc]initWithValues:createdAt_dict];
        id contentProvider_dict = dict[@"contentProvider"];
        if(contentProvider_dict != nil)
            _contentProvider = [[SWGContentProvider alloc]initWithValues:contentProvider_dict];
        _htmlExtra = dict[@"htmlExtra"]; 
        _word = dict[@"word"]; 
        id definitions_dict = dict[@"definitions"];
        if([definitions_dict isKindOfClass:[NSArray class]]) {

            NSMutableArray * objs = [[NSMutableArray alloc] initWithCapacity:[(NSArray*)definitions_dict count]];

            if([(NSArray*)definitions_dict count] > 0) {
                for (NSDictionary* dict in (NSArray*)definitions_dict) {
                    SWGSimpleDefinition* d = [[SWGSimpleDefinition alloc] initWithValues:dict];
                    [objs addObject:d];
                }
                
                _definitions = [[NSArray alloc] initWithArray:objs];
            }
            else {
                _definitions = [[NSArray alloc] init];
            }
        }
        else {
            _definitions = [[NSArray alloc] init];
        }
        id examples_dict = dict[@"examples"];
        if([examples_dict isKindOfClass:[NSArray class]]) {

            NSMutableArray * objs = [[NSMutableArray alloc] initWithCapacity:[(NSArray*)examples_dict count]];

            if([(NSArray*)examples_dict count] > 0) {
                for (NSDictionary* dict in (NSArray*)examples_dict) {
                    SWGSimpleExample* d = [[SWGSimpleExample alloc] initWithValues:dict];
                    [objs addObject:d];
                }
                
                _examples = [[NSArray alloc] initWithArray:objs];
            }
            else {
                _examples = [[NSArray alloc] init];
            }
        }
        else {
            _examples = [[NSArray alloc] init];
        }
        _note = dict[@"note"]; 
        id publishDate_dict = dict[@"publishDate"];
        if(publishDate_dict != nil)
            _publishDate = [[SWGDate alloc]initWithValues:publishDate_dict];
        

    }
    return self;
}

-(NSDictionary*) asDictionary {
    NSMutableDictionary* dict = [[NSMutableDictionary alloc] init];
    if(__id != nil) dict[@"id"] = __id ;
        if(_parentId != nil) dict[@"parentId"] = _parentId ;
        if(_category != nil) dict[@"category"] = _category ;
        if(_createdBy != nil) dict[@"createdBy"] = _createdBy ;
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
    if(_contentProvider != nil){
        if([_contentProvider isKindOfClass:[NSArray class]]){
            NSMutableArray * array = [[NSMutableArray alloc] init];
            for( SWGContentProvider *contentProvider in (NSArray*)_contentProvider) {
                [array addObject:[(SWGObject*)contentProvider asDictionary]];
            }
            dict[@"contentProvider"] = array;
        }
        else if(_contentProvider && [_contentProvider isKindOfClass:[SWGDate class]]) {
            NSString * dateString = [(SWGDate*)_contentProvider toString];
            if(dateString){
                dict[@"contentProvider"] = dateString;
            }
        }
        else {
        if(_contentProvider != nil) dict[@"contentProvider"] = [(SWGObject*)_contentProvider asDictionary];
        }
    }
    if(_htmlExtra != nil) dict[@"htmlExtra"] = _htmlExtra ;
        if(_word != nil) dict[@"word"] = _word ;
        if(_definitions != nil){
        if([_definitions isKindOfClass:[NSArray class]]){
            NSMutableArray * array = [[NSMutableArray alloc] init];
            for( SWGSimpleDefinition *definitions in (NSArray*)_definitions) {
                [array addObject:[(SWGObject*)definitions asDictionary]];
            }
            dict[@"definitions"] = array;
        }
        else if(_definitions && [_definitions isKindOfClass:[SWGDate class]]) {
            NSString * dateString = [(SWGDate*)_definitions toString];
            if(dateString){
                dict[@"definitions"] = dateString;
            }
        }
        else {
        if(_definitions != nil) dict[@"definitions"] = [(SWGObject*)_definitions asDictionary];
        }
    }
    if(_examples != nil){
        if([_examples isKindOfClass:[NSArray class]]){
            NSMutableArray * array = [[NSMutableArray alloc] init];
            for( SWGSimpleExample *examples in (NSArray*)_examples) {
                [array addObject:[(SWGObject*)examples asDictionary]];
            }
            dict[@"examples"] = array;
        }
        else if(_examples && [_examples isKindOfClass:[SWGDate class]]) {
            NSString * dateString = [(SWGDate*)_examples toString];
            if(dateString){
                dict[@"examples"] = dateString;
            }
        }
        else {
        if(_examples != nil) dict[@"examples"] = [(SWGObject*)_examples asDictionary];
        }
    }
    if(_note != nil) dict[@"note"] = _note ;
        if(_publishDate != nil){
        if([_publishDate isKindOfClass:[NSArray class]]){
            NSMutableArray * array = [[NSMutableArray alloc] init];
            for( SWGDate *publishDate in (NSArray*)_publishDate) {
                [array addObject:[(SWGObject*)publishDate asDictionary]];
            }
            dict[@"publishDate"] = array;
        }
        else if(_publishDate && [_publishDate isKindOfClass:[SWGDate class]]) {
            NSString * dateString = [(SWGDate*)_publishDate toString];
            if(dateString){
                dict[@"publishDate"] = dateString;
            }
        }
        else {
        if(_publishDate != nil) dict[@"publishDate"] = [(SWGObject*)_publishDate asDictionary];
        }
    }
    NSDictionary* output = [dict copy];
    return output;
}

@end

