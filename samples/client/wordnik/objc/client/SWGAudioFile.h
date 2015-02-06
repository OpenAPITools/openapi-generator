#import <Foundation/Foundation.h>
#import "SWGObject.h"
#import "SWGDate.h"


@interface SWGAudioFile : SWGObject

@property(nonatomic) NSString* attributionUrl;  
@property(nonatomic) NSNumber* commentCount;  
@property(nonatomic) NSNumber* voteCount;  
@property(nonatomic) NSString* fileUrl;  
@property(nonatomic) NSString* audioType;  
@property(nonatomic) NSNumber* _id;  
@property(nonatomic) NSNumber* duration;  
@property(nonatomic) NSString* attributionText;  
@property(nonatomic) NSString* createdBy;  
@property(nonatomic) NSString* _description;  
@property(nonatomic) SWGDate* createdAt;  
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
    _description: (NSString*) _description     
    createdAt: (SWGDate*) createdAt     
    voteWeightedAverage: (NSNumber*) voteWeightedAverage     
    voteAverage: (NSNumber*) voteAverage     
    word: (NSString*) word;
    

- (id) initWithValues: (NSDictionary*)dict;
- (NSDictionary*) asDictionary;

@end
