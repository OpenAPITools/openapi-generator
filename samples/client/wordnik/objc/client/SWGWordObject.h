#import <Foundation/Foundation.h>
#import "SWGObject.h"


@interface SWGWordObject : SWGObject

@property(nonatomic) NSNumber* _id;  
@property(nonatomic) NSString* word;  
@property(nonatomic) NSString* originalWord;  
@property(nonatomic) NSArray* suggestions;  
@property(nonatomic) NSString* canonicalForm;  
@property(nonatomic) NSString* vulgar;  
- (id) _id: (NSNumber*) _id     
    word: (NSString*) word     
    originalWord: (NSString*) originalWord     
    suggestions: (NSArray*) suggestions     
    canonicalForm: (NSString*) canonicalForm     
    vulgar: (NSString*) vulgar;
    

- (id) initWithValues: (NSDictionary*)dict;
- (NSDictionary*) asDictionary;

@end
