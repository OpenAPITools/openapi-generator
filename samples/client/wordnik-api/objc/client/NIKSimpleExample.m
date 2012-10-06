#import "NIKSimpleExample.h"

@implementation NIKSimpleExample

@synthesize _id = __id;
@synthesize text = _text;
@synthesize title = _title;
@synthesize url = _url;
- (id) _id: (NSNumber*) _id
       text: (NSString*) text
       title: (NSString*) title
       url: (NSString*) url
       {
          __id = _id;
          _text = text;
          _title = title;
          _url = url;
          return self;
       }

- (id) initWithValues: (NSDictionary*)dict
{
    __id = [dict objectForKey:@"id"];
    _text = [dict objectForKey:@"text"];
    _title = [dict objectForKey:@"title"];
    _url = [dict objectForKey:@"url"];
    return self;
}

-(NSDictionary*) asDictionary {
    NSMutableDictionary* dict = [[NSMutableDictionary alloc] init];
    if(__id != nil) [dict setObject:__id forKey:@"id"];
    if(_text != nil) [dict setObject:_text forKey:@"text"];
    if(_title != nil) [dict setObject:_title forKey:@"title"];
    if(_url != nil) [dict setObject:_url forKey:@"url"];
    NSDictionary* output = [dict copy];
    return output;
}

@end

