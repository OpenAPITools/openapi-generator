#import <Foundation/Foundation.h>
#import "NIKSwaggerObject.h"

@interface NIKBigram : NIKSwaggerObject {
@private
    NSNumber* _count; //NSNumber
    NSString* _gram2; //NSString
    NSString* _gram1; //NSString
    NSNumber* _wlmi; //NSNumber
    NSNumber* _mi; //NSNumber
    }



@property(nonatomic) NSNumber* count;
@property(nonatomic) NSString* gram2;
@property(nonatomic) NSString* gram1;
@property(nonatomic) NSNumber* wlmi;
@property(nonatomic) NSNumber* mi;
- (id) count: (NSNumber*) count
     gram2: (NSString*) gram2
     gram1: (NSString*) gram1
     wlmi: (NSNumber*) wlmi
     mi: (NSNumber*) mi;

- (id) initWithValues: (NSDictionary*)dict;
- (NSDictionary*) asDictionary;


@end

