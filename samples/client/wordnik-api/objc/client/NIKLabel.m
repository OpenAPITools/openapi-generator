#import "NIKDate.h"
#import "NIKLabel.h"

@implementation NIKLabel

@synthesize text = _text;
@synthesize type = _type;
- (id) text: (NSString*) text
       type: (NSString*) type
       {
          _text = text;
          _type = type;
          return self;
       }

- (id) initWithValues: (NSDictionary*)dict
{
    _text = [dict objectForKey:@"text"];
    _type = [dict objectForKey:@"type"];
    return self;
}

-(NSDictionary*) asDictionary {
    NSMutableDictionary* dict = [[NSMutableDictionary alloc] init];
    if(_text != nil) [dict setObject:_text forKey:@"text"];
    if(_type != nil) [dict setObject:_type forKey:@"type"];
    NSDictionary* output = [dict copy];
    return output;
}

@end

