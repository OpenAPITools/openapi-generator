#import <Foundation/Foundation.h>
#import "SWGObject.h"
#import "SWGDefinition.h"


@interface SWGDefinitionSearchResults : SWGObject

@property(nonatomic) NSArray* results;  
@property(nonatomic) NSNumber* totalResults;  
- (id) results: (NSArray*) results     
    totalResults: (NSNumber*) totalResults;
    

- (id) initWithValues: (NSDictionary*)dict;
- (NSDictionary*) asDictionary;

@end
