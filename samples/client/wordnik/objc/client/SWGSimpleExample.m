#import "SWGDate.h"
#import "SWGSimpleExample.h"

@implementation SWGSimpleExample

-(id)_id: (NSNumber*) _id
    title: (NSString*) title
    text: (NSString*) text
    url: (NSString*) url
    
{
    __id = _id;
    _title = title;
    _text = text;
    _url = url;
    

    return self;
}

-(id) initWithValues:(NSDictionary*)dict
{
    self = [super init];
    if(self) {
        __id = dict[@"id"];
        
        _title = dict[@"title"];
        
        _text = dict[@"text"];
        
        _url = dict[@"url"];
        
        
    }
    return self;
}

-(NSDictionary*) asDictionary {
    NSMutableDictionary* dict = [[NSMutableDictionary alloc] init];
    
    
            if(__id != nil) dict[@"id"] = __id ;
        
    
    
            if(_title != nil) dict[@"title"] = _title ;
        
    
    
            if(_text != nil) dict[@"text"] = _text ;
        
    
    
            if(_url != nil) dict[@"url"] = _url ;
        
    

    NSDictionary* output = [dict copy];
    return output;
}

@end
