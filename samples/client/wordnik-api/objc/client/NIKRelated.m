#import "NIKDate.h"
#import "NIKRelated.h"

@implementation NIKRelated

@synthesize label1 = _label1;
@synthesize relationshipType = _relationshipType;
@synthesize label2 = _label2;
@synthesize label3 = _label3;
@synthesize words = _words;
@synthesize gram = _gram;
@synthesize label4 = _label4;
- (id) label1: (NSString*) label1
       relationshipType: (NSString*) relationshipType
       label2: (NSString*) label2
       label3: (NSString*) label3
       words: (NSArray*) words
       gram: (NSString*) gram
       label4: (NSString*) label4
       {
          _label1 = label1;
          _relationshipType = relationshipType;
          _label2 = label2;
          _label3 = label3;
          _words = words;
          _gram = gram;
          _label4 = label4;
          return self;
       }

- (id) initWithValues: (NSDictionary*)dict
{
    _label1 = [dict objectForKey:@"label1"];
    _relationshipType = [dict objectForKey:@"relationshipType"];
    _label2 = [dict objectForKey:@"label2"];
    _label3 = [dict objectForKey:@"label3"];
    _words = [dict objectForKey:@"words"];
    _gram = [dict objectForKey:@"gram"];
    _label4 = [dict objectForKey:@"label4"];
    return self;
}

-(NSDictionary*) asDictionary {
    NSMutableDictionary* dict = [[NSMutableDictionary alloc] init];
    if(_label1 != nil) [dict setObject:_label1 forKey:@"label1"];
    if(_relationshipType != nil) [dict setObject:_relationshipType forKey:@"relationshipType"];
    if(_label2 != nil) [dict setObject:_label2 forKey:@"label2"];
    if(_label3 != nil) [dict setObject:_label3 forKey:@"label3"];
    if(_words != nil) [dict setObject:_words forKey:@"words"];
    if(_gram != nil) [dict setObject:_gram forKey:@"gram"];
    if(_label4 != nil) [dict setObject:_label4 forKey:@"label4"];
    NSDictionary* output = [dict copy];
    return output;
}

@end

