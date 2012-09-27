#import <Foundation/Foundation.h>
#import "NIKSwaggerObject.h"
#import "NIKFrequency.h"

@interface NIKFrequencySummary : NIKSwaggerObject {
@private
    NSNumber* _unknownYearCount; //NSNumber
    NSNumber* _totalCount; //NSNumber
    NSString* _frequencyString; //NSString
    NSString* _word; //NSString
    NSArray* _frequency; //Frequency
    }



@property(nonatomic) NSNumber* unknownYearCount;
@property(nonatomic) NSNumber* totalCount;
@property(nonatomic) NSString* frequencyString;
@property(nonatomic) NSString* word;
@property(nonatomic) NSArray* frequency;
- (id) unknownYearCount: (NSNumber*) unknownYearCount
     totalCount: (NSNumber*) totalCount
     frequencyString: (NSString*) frequencyString
     word: (NSString*) word
     frequency: (NSArray*) frequency;

- (id) initWithValues: (NSDictionary*)dict;
- (NSDictionary*) asDictionary;


@end

