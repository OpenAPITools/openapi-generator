#import "EXPFloatTuple.h"

@implementation EXPFloatTuple

@synthesize values = _values, size = _size;

- (instancetype)initWithFloatValues:(float *)values size:(size_t)size {
    if ((self = [super init])) {
        self.values = malloc(sizeof(float) * size);
        memcpy(self.values, values, sizeof(float) * size);
        self.size = size;
    }
    return self;
}

- (void)dealloc {
    free(self.values);
    [super dealloc];
}

- (BOOL)isEqual:(id)object {
    if (![object isKindOfClass:[EXPFloatTuple class]]) return NO;
    EXPFloatTuple *other = (EXPFloatTuple *)object;
    if (self.size == other.size) {
        for (int i = 0; i < self.size; ++i) {
            if (self.values[i] != other.values[i]) return NO;
        }
        return YES;
    }
    return NO;
}

- (NSUInteger)hash
{
    NSUInteger prime = 31;
    NSUInteger hash = 0;
    for (int i=0; i<self.size; i++) {
        hash = prime * hash + (NSUInteger)self.values[i];
    }
    return hash;
}

- (NSString *)description {
    if (self.size == 2) {
        return [NSString stringWithFormat:@"Float tuple: {%f, %f}", self.values[0], self.values[1]];
    } else if (self.size == 4) {
        return [NSString stringWithFormat:@"Float tuple: {%f, %f, %f, %f}", self.values[0], self.values[1], self.values[2], self.values[3]];
    }
    return [NSString stringWithFormat:@"Float tuple of unexpected size %zd, sadly", self.size];
}

@end
