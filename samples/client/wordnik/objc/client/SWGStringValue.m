#import "SWGDate.h"
#import "SWGStringValue.h"

@implementation SWGStringValue

-(id)word: (NSString*) word
    
{
    _word = word;
    

    return self;
}

-(id) initWithValues:(NSDictionary*)dict
{
    self = [super init];
    if(self) {
        _word = dict[@"word"];
        
        
    }
    return self;
}

-(NSDictionary*) asDictionary {
    NSMutableDictionary* dict = [[NSMutableDictionary alloc] init];
    
    
            if(_word != nil) dict[@"word"] = _word ;
        
    

    NSDictionary* output = [dict copy];
    return output;
}

@end
