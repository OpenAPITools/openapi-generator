#import <Foundation/Foundation.h>
#import "SWGObject.h"
#import "SWGLabel.h"
#import "SWGExampleUsage.h"
#import "SWGTextPron.h"
#import "SWGCitation.h"
#import "SWGRelated.h"
#import "SWGNote.h"


@interface SWGDefinition : SWGObject

@property(nonatomic) NSString* extendedText;  

@property(nonatomic) NSString* text;  

@property(nonatomic) NSString* sourceDictionary;  

@property(nonatomic) NSArray* citations;  

@property(nonatomic) NSArray* labels;  

@property(nonatomic) NSNumber* score;  

@property(nonatomic) NSArray* exampleUses;  

@property(nonatomic) NSString* attributionUrl;  

@property(nonatomic) NSString* seqString;  

@property(nonatomic) NSString* attributionText;  

@property(nonatomic) NSArray* relatedWords;  

@property(nonatomic) NSString* sequence;  

@property(nonatomic) NSString* word;  

@property(nonatomic) NSArray* notes;  

@property(nonatomic) NSArray* textProns;  

@property(nonatomic) NSString* partOfSpeech;  

- (id) extendedText: (NSString*) extendedText
     text: (NSString*) text
     sourceDictionary: (NSString*) sourceDictionary
     citations: (NSArray*) citations
     labels: (NSArray*) labels
     score: (NSNumber*) score
     exampleUses: (NSArray*) exampleUses
     attributionUrl: (NSString*) attributionUrl
     seqString: (NSString*) seqString
     attributionText: (NSString*) attributionText
     relatedWords: (NSArray*) relatedWords
     sequence: (NSString*) sequence
     word: (NSString*) word
     notes: (NSArray*) notes
     textProns: (NSArray*) textProns
     partOfSpeech: (NSString*) partOfSpeech;

- (id) initWithValues: (NSDictionary*)dict;
- (NSDictionary*) asDictionary;


@end

