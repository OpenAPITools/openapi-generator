#import "SWGDate.h"
#import "SWGNote.h"

@implementation SWGNote

-(id)noteType: (NSString*) noteType
    appliesTo: (NSArray*) appliesTo
    value: (NSString*) value
    pos: (NSNumber*) pos
    
{
    _noteType = noteType;
    _appliesTo = appliesTo;
    _value = value;
    _pos = pos;
    

    return self;
}

-(id) initWithValues:(NSDictionary*)dict
{
    self = [super init];
    if(self) {
        _noteType = dict[@"noteType"];
        
        _appliesTo = dict[@"appliesTo"];
        
        _value = dict[@"value"];
        
        _pos = dict[@"pos"];
        
        
    }
    return self;
}

-(NSDictionary*) asDictionary {
    NSMutableDictionary* dict = [[NSMutableDictionary alloc] init];
    
    
            if(_noteType != nil) dict[@"noteType"] = _noteType ;
        
    
    
            if(_appliesTo != nil) dict[@"appliesTo"] = _appliesTo ;
        
    
    
            if(_value != nil) dict[@"value"] = _value ;
        
    
    
            if(_pos != nil) dict[@"pos"] = _pos ;
        
    

    NSDictionary* output = [dict copy];
    return output;
}

@end
