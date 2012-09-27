#import "NIKDate.h"
#import "NIKSyllable.h"

@implementation NIKSyllable

@synthesize text = _text;
@synthesize seq = _seq;
@synthesize type = _type;
- (id) text: (NSString*) text
       seq: (NSNumber*) seq
       type: (NSString*) type
       {
          _text = text;
          _seq = seq;
          _type = type;
          return self;
       }

- (id) initWithValues: (NSDictionary*)dict
{
    /* isContainer: , baseType: NSString, complexType:  */
    _text = [dict objectForKey:@"text"];
    /* isContainer: , baseType: NSNumber, complexType:  */
    _seq = [dict objectForKey:@"seq"];
    /* isContainer: , baseType: NSString, complexType:  */
    _type = [dict objectForKey:@"type"];
    return self;
}

-(NSDictionary*) asDictionary {
    NSMutableDictionary* dict = [[NSMutableDictionary alloc] init];
    if(_text != nil) [dict setObject:_text forKey:@"text"];
    if(_seq != nil) [dict setObject:_seq forKey:@"seq"];
    if(_type != nil) [dict setObject:_type forKey:@"type"];
    NSDictionary* output = [dict copy];
    return output;
}

@end

