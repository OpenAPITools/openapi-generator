#import "NIKDate.h"
#import "NIKNote.h"

@implementation NIKNote

@synthesize noteType = _noteType;
@synthesize appliesTo = _appliesTo;
@synthesize value = _value;
@synthesize pos = _pos;
- (id) noteType: (NSString*) noteType
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

- (id) initWithValues: (NSDictionary*)dict
{
    /* isContainer: , baseType: NSString, complexType:  */
    _noteType = [dict objectForKey:@"noteType"];
    /* isContainer: true, baseType: NSString, complexType:  */
    _appliesTo = [dict objectForKey:@"appliesTo"];
    /* isContainer: , baseType: NSString, complexType:  */
    _value = [dict objectForKey:@"value"];
    /* isContainer: , baseType: NSNumber, complexType:  */
    _pos = [dict objectForKey:@"pos"];
    return self;
}

-(NSDictionary*) asDictionary {
    NSMutableDictionary* dict = [[NSMutableDictionary alloc] init];
    if(_noteType != nil) [dict setObject:_noteType forKey:@"noteType"];
    if(_appliesTo != nil) [dict setObject:_appliesTo forKey:@"appliesTo"];
    if(_value != nil) [dict setObject:_value forKey:@"value"];
    if(_pos != nil) [dict setObject:_pos forKey:@"pos"];
    NSDictionary* output = [dict copy];
    return output;
}

@end

