#import <Foundation/Foundation.h>
#import "SWGObject.h"
#import "SWGSentence.h"
#import "SWGContentProvider.h"
#import "SWGScoredWord.h"


@interface SWGExample : SWGObject

@property(nonatomic) NSNumber* _id;  

@property(nonatomic) NSNumber* exampleId;  

@property(nonatomic) NSString* title;  

@property(nonatomic) NSString* text;  

@property(nonatomic) SWGScoredWord* score;  

@property(nonatomic) SWGSentence* sentence;  

@property(nonatomic) NSString* word;  

@property(nonatomic) SWGContentProvider* provider;  

@property(nonatomic) NSNumber* year;  

@property(nonatomic) NSNumber* rating;  

@property(nonatomic) NSNumber* documentId;  

@property(nonatomic) NSString* url;  

- (id) _id: (NSNumber*) _id
     exampleId: (NSNumber*) exampleId
     title: (NSString*) title
     text: (NSString*) text
     score: (SWGScoredWord*) score
     sentence: (SWGSentence*) sentence
     word: (NSString*) word
     provider: (SWGContentProvider*) provider
     year: (NSNumber*) year
     rating: (NSNumber*) rating
     documentId: (NSNumber*) documentId
     url: (NSString*) url;

- (id) initWithValues: (NSDictionary*)dict;
- (NSDictionary*) asDictionary;


@end

