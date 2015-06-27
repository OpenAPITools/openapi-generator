#import <Foundation/Foundation.h>

@interface EXPFloatTuple : NSObject {
    float *_values;
    size_t _size;
}

@property (nonatomic, assign) float *values;
@property (nonatomic, assign) size_t size;

- (instancetype)initWithFloatValues:(float *)values size:(size_t)size NS_DESIGNATED_INITIALIZER;

@end
