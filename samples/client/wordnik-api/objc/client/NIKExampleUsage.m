#import "NIKDate.h"
#import "NIKExampleUsage.h"

@implementation NIKExampleUsage

@synthesize text = _text;
- (id) text: (NSString*) text
       {
          _text = text;
          return self;
       }

- (id) initWithValues: (NSDictionary*)dict
{
    _text = [dict objectForKey:@"text"];
    return self;
}

-(NSDictionary*) asDictionary {
    NSMutableDictionary* dict = [[NSMutableDictionary alloc] init];
    if(_text != nil) [dict setObject:_text forKey:@"text"];
    NSDictionary* output = [dict copy];
    return output;
}

@end

