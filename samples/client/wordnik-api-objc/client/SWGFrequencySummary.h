#import <Foundation/Foundation.h>
#import "SWGObject.h"
#import "SWGFrequency.h"


@interface SWGFrequencySummary : SWGObject

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

