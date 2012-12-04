#import "NIKDate.h"
#import "NIKSimpleExample.h"

@implementation NIKSimpleExample

@synthesize _id = __id;
@synthesize title = _title;
@synthesize text = _text;
@synthesize url = _url;
- (id) _id: (NSNumber*) _id
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

- (id) initWithValues: (NSDictionary*)dict
{
    __id = [dict objectForKey:@"id"];
    _title = [dict objectForKey:@"title"];
    _text = [dict objectForKey:@"text"];
    _url = [dict objectForKey:@"url"];
    return self;
}

-(NSDictionary*) asDictionary {
    NSMutableDictionary* dict = [[NSMutableDictionary alloc] init];
    if(__id != nil) [dict setObject:__id forKey:@"id"];
    if(_title != nil) [dict setObject:_title forKey:@"title"];
    if(_text != nil) [dict setObject:_text forKey:@"text"];
    if(_url != nil) [dict setObject:_url forKey:@"url"];
    NSDictionary* output = [dict copy];
    return output;
}

@end

