#import "SWGDate.h"
#import "SWGOrders.h"

@implementation SWGOrders

-(id)
    
    return self;
}

-(id) initWithValues:(NSDictionary*)dict
{
    self = [super init];
    if(self) {
        
    }
    return self;
}

-(NSDictionary*) asDictionary {
    NSMutableDictionary* dict = [[NSMutableDictionary alloc] init];
    
    NSDictionary* output = [dict copy];
    return output;
}

@end
