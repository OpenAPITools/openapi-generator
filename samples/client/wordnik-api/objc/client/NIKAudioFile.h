#import <Foundation/Foundation.h>
#import "NIKSwaggerObject.h"
#import "NIKDate.h"

@interface NIKAudioFile : NIKSwaggerObject {
@private
    NSString* _attributionUrl; //NSString
    NSNumber* _commentCount; //NSNumber
    NSNumber* _voteCount; //NSNumber
    NSString* _fileUrl; //NSString
    NSString* _audioType; //NSString
    NSNumber* __id; //NSNumber
    NSNumber* _duration; //NSNumber
    NSString* _attributionText; //NSString
    NSString* _createdBy; //NSString
    NSString* _description; //NSString
    NIKDate* _createdAt; //NIKDate
    NSNumber* _voteWeightedAverage; //NSNumber
    NSNumber* _voteAverage; //NSNumber
    NSString* _word; //NSString
    }



@property(nonatomic) NSString* attributionUrl;
@property(nonatomic) NSNumber* commentCount;
@property(nonatomic) NSNumber* voteCount;
@property(nonatomic) NSString* fileUrl;
@property(nonatomic) NSString* audioType;
@property(nonatomic) NSNumber* _id;
@property(nonatomic) NSNumber* duration;
@property(nonatomic) NSString* attributionText;
@property(nonatomic) NSString* createdBy;
@property(nonatomic) NSString* description;
@property(nonatomic) NIKDate* createdAt;
@property(nonatomic) NSNumber* voteWeightedAverage;
@property(nonatomic) NSNumber* voteAverage;
@property(nonatomic) NSString* word;
- (id) attributionUrl: (NSString*) attributionUrl
     commentCount: (NSNumber*) commentCount
     voteCount: (NSNumber*) voteCount
     fileUrl: (NSString*) fileUrl
     audioType: (NSString*) audioType
     _id: (NSNumber*) _id
     duration: (NSNumber*) duration
     attributionText: (NSString*) attributionText
     createdBy: (NSString*) createdBy
     description: (NSString*) description
     createdAt: (NIKDate*) createdAt
     voteWeightedAverage: (NSNumber*) voteWeightedAverage
     voteAverage: (NSNumber*) voteAverage
     word: (NSString*) word;

- (id) initWithValues: (NSDictionary*)dict;
- (NSDictionary*) asDictionary;


@end

