#import "NIKWordOfTheDay.h"

@implementation NIKWordOfTheDay

@synthesize _id = __id;
@synthesize parentId = _parentId;
@synthesize category = _category;
@synthesize createdBy = _createdBy;
@synthesize createdAt = _createdAt;
@synthesize contentProvider = _contentProvider;
@synthesize word = _word;
@synthesize htmlExtra = _htmlExtra;
@synthesize definitions = _definitions;
@synthesize examples = _examples;
@synthesize publishDate = _publishDate;
@synthesize note = _note;
- (id) _id: (NSNumber*) _id
       parentId: (NSString*) parentId
       category: (NSString*) category
       createdBy: (NSString*) createdBy
       createdAt: (NIKDate*) createdAt
       contentProvider: (NIKContentProvider*) contentProvider
       word: (NSString*) word
       htmlExtra: (NSString*) htmlExtra
       definitions: (NSArray*) definitions
       examples: (NSArray*) examples
       publishDate: (NIKDate*) publishDate
       note: (NSString*) note
       {
          __id = _id;
          _parentId = parentId;
          _category = category;
          _createdBy = createdBy;
          _createdAt = createdAt;
          _contentProvider = contentProvider;
          _word = word;
          _htmlExtra = htmlExtra;
          _definitions = definitions;
          _examples = examples;
          _publishDate = publishDate;
          _note = note;
          return self;
       }

- (id) initWithValues: (NSDictionary*)dict
{
    __id = [dict objectForKey:@"id"];
    _parentId = [dict objectForKey:@"parentId"];
    _category = [dict objectForKey:@"category"];
    _createdBy = [dict objectForKey:@"createdBy"];
    id createdAt_dict = [dict objectForKey:@"createdAt"];
    _createdAt = [[NIKDate alloc]initWithValues:createdAt_dict];
    id contentProvider_dict = [dict objectForKey:@"contentProvider"];
    _contentProvider = [[NIKContentProvider alloc]initWithValues:contentProvider_dict];
    _word = [dict objectForKey:@"word"];
    _htmlExtra = [dict objectForKey:@"htmlExtra"];
    id definitions_dict = [dict objectForKey:@"definitions"];
    if([definitions_dict isKindOfClass:[NSArray class]]) {
        if([(NSArray*)definitions_dict count] > 0) {
            NSMutableArray * objs = [[NSMutableArray alloc] initWithCapacity:[(NSArray*)definitions_dict count]];
            for (NSDictionary* dict in (NSArray*)definitions_dict) {
                NIKSimpleDefinition* d = [[NIKSimpleDefinition alloc]initWithValues:dict];
                [objs addObject:d];
            }
            _definitions = [[NSArray alloc] initWithArray:objs];
        }
    }
    id examples_dict = [dict objectForKey:@"examples"];
    if([examples_dict isKindOfClass:[NSArray class]]) {
        if([(NSArray*)examples_dict count] > 0) {
            NSMutableArray * objs = [[NSMutableArray alloc] initWithCapacity:[(NSArray*)examples_dict count]];
            for (NSDictionary* dict in (NSArray*)examples_dict) {
                NIKSimpleExample* d = [[NIKSimpleExample alloc]initWithValues:dict];
                [objs addObject:d];
            }
            _examples = [[NSArray alloc] initWithArray:objs];
        }
    }
    id publishDate_dict = [dict objectForKey:@"publishDate"];
    _publishDate = [[NIKDate alloc]initWithValues:publishDate_dict];
    _note = [dict objectForKey:@"note"];
    return self;
}

-(NSDictionary*) asDictionary {
    NSMutableDictionary* dict = [[NSMutableDictionary alloc] init];
    if(__id != nil) [dict setObject:__id forKey:@"id"];
    if(_parentId != nil) [dict setObject:_parentId forKey:@"parentId"];
    if(_category != nil) [dict setObject:_category forKey:@"category"];
    if(_createdBy != nil) [dict setObject:_createdBy forKey:@"createdBy"];
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
    if(_contentProvider != nil){
        if([_contentProvider isKindOfClass:[NSArray class]]){
            NSMutableArray * array = [[NSMutableArray alloc] init];
            for( NIKContentProvider * contentProvider in (NSArray*)_contentProvider) {
                [array addObject:[(NIKSwaggerObject*)contentProvider asDictionary]];
            }
            [dict setObject:array forKey:@"contentProvider"];
        }
        else if(_contentProvider && [_contentProvider isKindOfClass:[NIKDate class]]) {
            NSString * dateString = [(NIKDate*)_contentProvider toString];
            if(dateString){
                [dict setObject:dateString forKey:@"contentProvider"];   
            }
        }
    }
    else {
    if(_contentProvider != nil) [dict setObject:[(NIKSwaggerObject*)_contentProvider asDictionary]forKey:@"contentProvider"];
    }
    if(_word != nil) [dict setObject:_word forKey:@"word"];
    if(_htmlExtra != nil) [dict setObject:_htmlExtra forKey:@"htmlExtra"];
    if(_definitions != nil){
        if([_definitions isKindOfClass:[NSArray class]]){
            NSMutableArray * array = [[NSMutableArray alloc] init];
            for( NIKSimpleDefinition * definitions in (NSArray*)_definitions) {
                [array addObject:[(NIKSwaggerObject*)definitions asDictionary]];
            }
            [dict setObject:array forKey:@"definitions"];
        }
        else if(_definitions && [_definitions isKindOfClass:[NIKDate class]]) {
            NSString * dateString = [(NIKDate*)_definitions toString];
            if(dateString){
                [dict setObject:dateString forKey:@"definitions"];   
            }
        }
    }
    else {
    if(_definitions != nil) [dict setObject:[(NIKSwaggerObject*)_definitions asDictionary]forKey:@"definitions"];
    }
    if(_examples != nil){
        if([_examples isKindOfClass:[NSArray class]]){
            NSMutableArray * array = [[NSMutableArray alloc] init];
            for( NIKSimpleExample * examples in (NSArray*)_examples) {
                [array addObject:[(NIKSwaggerObject*)examples asDictionary]];
            }
            [dict setObject:array forKey:@"examples"];
        }
        else if(_examples && [_examples isKindOfClass:[NIKDate class]]) {
            NSString * dateString = [(NIKDate*)_examples toString];
            if(dateString){
                [dict setObject:dateString forKey:@"examples"];   
            }
        }
    }
    else {
    if(_examples != nil) [dict setObject:[(NIKSwaggerObject*)_examples asDictionary]forKey:@"examples"];
    }
    if(_publishDate != nil){
        if([_publishDate isKindOfClass:[NSArray class]]){
            NSMutableArray * array = [[NSMutableArray alloc] init];
            for( NIKDate * publishDate in (NSArray*)_publishDate) {
                [array addObject:[(NIKSwaggerObject*)publishDate asDictionary]];
            }
            [dict setObject:array forKey:@"publishDate"];
        }
        else if(_publishDate && [_publishDate isKindOfClass:[NIKDate class]]) {
            NSString * dateString = [(NIKDate*)_publishDate toString];
            if(dateString){
                [dict setObject:dateString forKey:@"publishDate"];   
            }
        }
    }
    else {
    if(_publishDate != nil) [dict setObject:[(NIKSwaggerObject*)_publishDate asDictionary]forKey:@"publishDate"];
    }
    if(_note != nil) [dict setObject:_note forKey:@"note"];
    NSDictionary* output = [dict copy];
    return output;
}

@end

