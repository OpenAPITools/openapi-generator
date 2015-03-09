#import "SWGDate.h"
#import "SWGRelated.h"

@implementation SWGRelated

-(id)label1: (NSString*) label1
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

-(id) initWithValues:(NSDictionary*)dict
{
    self = [super init];
    if(self) {
        _label1 = dict[@"label1"];
        
        _relationshipType = dict[@"relationshipType"];
        
        _label2 = dict[@"label2"];
        
        _label3 = dict[@"label3"];
        
        _words = dict[@"words"];
        
        _gram = dict[@"gram"];
        
        _label4 = dict[@"label4"];
        
        
    }
    return self;
}

-(NSDictionary*) asDictionary {
    NSMutableDictionary* dict = [[NSMutableDictionary alloc] init];
    
    
            if(_label1 != nil) dict[@"label1"] = _label1 ;
        
    
    
            if(_relationshipType != nil) dict[@"relationshipType"] = _relationshipType ;
        
    
    
            if(_label2 != nil) dict[@"label2"] = _label2 ;
        
    
    
            if(_label3 != nil) dict[@"label3"] = _label3 ;
        
    
    
            if(_words != nil) dict[@"words"] = _words ;
        
    
    
            if(_gram != nil) dict[@"gram"] = _gram ;
        
    
    
            if(_label4 != nil) dict[@"label4"] = _label4 ;
        
    

    NSDictionary* output = [dict copy];
    return output;
}

@end
