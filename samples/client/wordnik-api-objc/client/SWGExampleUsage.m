#import "SWGDate.h"
#import "SWGExampleUsage.h"

@implementation SWGExampleUsage

-(id)text: (NSString*) text
{
  _text = text;
  return self;
}

-(id) initWithValues:(NSDictionary*)dict
{
    self = [super init];
    if(self) {
        _text = dict[@"text"]; 
        

    }
    return self;
}

-(NSDictionary*) asDictionary {
    NSMutableDictionary* dict = [[NSMutableDictionary alloc] init];
    if(_text != nil) dict[@"text"] = _text ;
        NSDictionary* output = [dict copy];
    return output;
}

@end

