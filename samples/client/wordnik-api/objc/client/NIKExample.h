#import <Foundation/Foundation.h>
#import "NIKSwaggerObject.h"
#import "NIKSentence.h"
#import "NIKScoredWord.h"
#import "NIKContentProvider.h"

@interface NIKExample : NIKSwaggerObject {
@private
    NSNumber* __id; //NSNumber
    NSNumber* _exampleId; //NSNumber
    NSString* _title; //NSString
    NSString* _text; //NSString
    NIKScoredWord* _score; //ScoredWord
    NIKSentence* _sentence; //Sentence
    NSString* _word; //NSString
    NIKContentProvider* _provider; //ContentProvider
    NSNumber* _year; //NSNumber
    NSNumber* _rating; //NSNumber
    NSNumber* _documentId; //NSNumber
    NSString* _url; //NSString
    }



@property(nonatomic) NSNumber* _id;
@property(nonatomic) NSNumber* exampleId;
@property(nonatomic) NSString* title;
@property(nonatomic) NSString* text;
@property(nonatomic) NIKScoredWord* score;
@property(nonatomic) NIKSentence* sentence;
@property(nonatomic) NSString* word;
@property(nonatomic) NIKContentProvider* provider;
@property(nonatomic) NSNumber* year;
@property(nonatomic) NSNumber* rating;
@property(nonatomic) NSNumber* documentId;
@property(nonatomic) NSString* url;
- (id) _id: (NSNumber*) _id
     exampleId: (NSNumber*) exampleId
     title: (NSString*) title
     text: (NSString*) text
     score: (NIKScoredWord*) score
     sentence: (NIKSentence*) sentence
     word: (NSString*) word
     provider: (NIKContentProvider*) provider
     year: (NSNumber*) year
     rating: (NSNumber*) rating
     documentId: (NSNumber*) documentId
     url: (NSString*) url;

- (id) initWithValues: (NSDictionary*)dict;
- (NSDictionary*) asDictionary;


@end

