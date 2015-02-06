#import <Foundation/Foundation.h>
#import "SWGObject.h"
#import "SWGScoredWord.h"


@interface SWGSentence : SWGObject

@property(nonatomic) NSNumber* hasScoredWords;  
@property(nonatomic) NSNumber* _id;  
@property(nonatomic) NSArray* scoredWords;  
@property(nonatomic) NSString* display;  
@property(nonatomic) NSNumber* rating;  
@property(nonatomic) NSNumber* documentMetadataId;  
- (id) hasScoredWords: (NSNumber*) hasScoredWords     
    _id: (NSNumber*) _id     
    scoredWords: (NSArray*) scoredWords     
    display: (NSString*) display     
    rating: (NSNumber*) rating     
    documentMetadataId: (NSNumber*) documentMetadataId;
    

- (id) initWithValues: (NSDictionary*)dict;
- (NSDictionary*) asDictionary;

@end
