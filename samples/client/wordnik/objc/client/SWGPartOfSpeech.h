#import <Foundation/Foundation.h>
#import "SWGObject.h"
#import "SWGRoot.h"
#import "SWGCategory.h"


@interface SWGPartOfSpeech : SWGObject

@property(nonatomic) NSArray* roots;  
@property(nonatomic) NSArray* storageAbbr;  
@property(nonatomic) NSArray* allCategories;  
- (id) roots: (NSArray*) roots     
    storageAbbr: (NSArray*) storageAbbr     
    allCategories: (NSArray*) allCategories;
    

- (id) initWithValues: (NSDictionary*)dict;
- (NSDictionary*) asDictionary;

@end
