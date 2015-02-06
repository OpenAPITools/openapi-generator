#import "SWGDate.h"
#import "SWGCitation.h"

@implementation SWGCitation

-(id)cite: (NSString*) cite
    source: (NSString*) source
    
{
    _cite = cite;
    _source = source;
    

    return self;
}

-(id) initWithValues:(NSDictionary*)dict
{
    self = [super init];
    if(self) {
        _cite = dict[@"cite"];
        
        _source = dict[@"source"];
        
        
    }
    return self;
}

-(NSDictionary*) asDictionary {
    NSMutableDictionary* dict = [[NSMutableDictionary alloc] init];
    
    
            if(_cite != nil) dict[@"cite"] = _cite ;
        
    
    
            if(_source != nil) dict[@"source"] = _source ;
        
    

    NSDictionary* output = [dict copy];
    return output;
}

@end
