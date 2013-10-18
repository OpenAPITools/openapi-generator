#import "SWGDate.h"

@implementation SWGDate

@synthesize date = _date;

- (id) initWithValues:(NSString*)input {
    if([input isKindOfClass:[NSString class]]){
        NSDateFormatter* df = [NSDateFormatter new];
        NSLocale *locale = [[NSLocale new]                        
                            initWithLocaleIdentifier:@"en_US_POSIX"];
        [df setLocale:locale];
        [df setDateFormat:@"yyyy-MM-dd'T'HH:mm:ss.SSSZ"];

        _date = [df dateFromString:input];
    }
    else if([input isKindOfClass:[NSNumber class]]) {
        NSTimeInterval interval = [input doubleValue];
        _date = [[NSDate alloc] initWithTimeIntervalSince1970:interval];        
    }
    return self;
}

-(NSString*) toString {
    NSDateFormatter* df = [NSDateFormatter new];
    NSLocale *locale = [[NSLocale new]                        
                        initWithLocaleIdentifier:@"en_US_POSIX"];
    [df setLocale:locale];
    [df setDateFormat:@"yyyy-MM-dd'T'HH:mm:ss.SSSZ"];
    
    return [df stringFromDate:_date];
}

@end