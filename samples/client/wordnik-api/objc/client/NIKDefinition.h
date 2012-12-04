#import <Foundation/Foundation.h>
#import "NIKSwaggerObject.h"
#import "NIKExampleUsage.h"
#import "NIKNote.h"
#import "NIKCitation.h"
#import "NIKTextPron.h"
#import "NIKLabel.h"
#import "NIKRelated.h"

@interface NIKDefinition : NIKSwaggerObject {
@private
    NSString* _extendedText; //NSString
    NSString* _text; //NSString
    NSString* _sourceDictionary; //NSString
    NSArray* _citations; //Citation
    NSArray* _labels; //Label
    NSNumber* _score; //NSNumber
    NSArray* _exampleUses; //ExampleUsage
    NSString* _attributionUrl; //NSString
    NSString* _seqString; //NSString
    NSString* _attributionText; //NSString
    NSArray* _relatedWords; //Related
    NSString* _sequence; //NSString
    NSString* _word; //NSString
    NSArray* _notes; //Note
    NSArray* _textProns; //TextPron
    NSString* _partOfSpeech; //NSString
    }



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

