#import <Foundation/Foundation.h>

@interface EXPDoubleTuple : NSObject {
    double *_values;
    size_t _size;
}

@property (nonatomic, assign) double *values;
@property (nonatomic, assign) size_t size;

- (instancetype)initWithDoubleValues:(double *)values size:(size_t)size NS_DESIGNATED_INITIALIZER;

@end
