#import "SWGQueryParamCollection.h"

@implementation SWGQueryParamCollection

@synthesize values = _values;
@synthesize format = _format;

- (id)initWithValuesAndFormat:(NSArray *)values
                       format:(NSString *)format {

    self = [super init];
    if (self) {
    	_values = values;
    	_format = format;
	}
    
    return self;
}

@end
