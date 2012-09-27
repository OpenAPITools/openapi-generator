#import <Foundation/Foundation.h>
#import "NIKSwaggerObject.h"
#import "NIKSentence.h"
#import "NIKScoredWord.h"
#import "NIKContentProvider.h"

@interface NIKExample : NIKSwaggerObject {
@private
    NSNumber* __id; //NSNumber
    NSString* _text; //NSString
    NSString* _title; //NSString
    NSNumber* _exampleId; //NSNumber
    NIKScoredWord* _score; //ScoredWord
    NIKSentence* _sentence; //Sentence
    NSNumber* _year; //NSNumber
    NIKContentProvider* _provider; //ContentProvider
    NSString* _word; //NSString
    NSNumber* _rating; //NSNumber
    NSString* _url; //NSString
    NSNumber* _documentId; //NSNumber
    }



@property(nonatomic) NSNumber* _id;
@property(nonatomic) NSString* text;
@property(nonatomic) NSString* title;
@property(nonatomic) NSNumber* exampleId;
@property(nonatomic) NIKScoredWord* score;
@property(nonatomic) NIKSentence* sentence;
@property(nonatomic) NSNumber* year;
@property(nonatomic) NIKContentProvider* provider;
@property(nonatomic) NSString* word;
@property(nonatomic) NSNumber* rating;
@property(nonatomic) NSString* url;
@property(nonatomic) NSNumber* documentId;
- (id) _id: (NSNumber*) _id
     text: (NSString*) text
     title: (NSString*) title
     exampleId: (NSNumber*) exampleId
     score: (NIKScoredWord*) score
     sentence: (NIKSentence*) sentence
     year: (NSNumber*) year
     provider: (NIKContentProvider*) provider
     word: (NSString*) word
     rating: (NSNumber*) rating
     url: (NSString*) url
     documentId: (NSNumber*) documentId;

- (id) initWithValues: (NSDictionary*)dict;
- (NSDictionary*) asDictionary;


@end

