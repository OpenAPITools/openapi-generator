#import "NIKDate.h"
#import "NIKStringValue.h"

@implementation NIKStringValue

@synthesize word = _word;
- (id) word: (NSString*) word
       {
          _word = word;
          return self;
       }

- (id) initWithValues: (NSDictionary*)dict
{
    _word = [dict objectForKey:@"word"];
    return self;
}

-(NSDictionary*) asDictionary {
    NSMutableDictionary* dict = [[NSMutableDictionary alloc] init];
    if(_word != nil) [dict setObject:_word forKey:@"word"];
    NSDictionary* output = [dict copy];
    return output;
}

@end

