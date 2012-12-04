#import <Foundation/Foundation.h>
#import "NIKSwaggerObject.h"
#import "NIKScoredWord.h"

@interface NIKSentence : NIKSwaggerObject {
@private
    NSNumber* _hasScoredWords; //NSNumber
    NSNumber* __id; //NSNumber
    NSArray* _scoredWords; //ScoredWord
    NSString* _display; //NSString
    NSNumber* _rating; //NSNumber
    NSNumber* _documentMetadataId; //NSNumber
    }



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

