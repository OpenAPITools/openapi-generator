#import "NIKDate.h"
#import "NIKAudioFile.h"

@implementation NIKAudioFile

@synthesize attributionUrl = _attributionUrl;
@synthesize commentCount = _commentCount;
@synthesize voteCount = _voteCount;
@synthesize fileUrl = _fileUrl;
@synthesize audioType = _audioType;
@synthesize _id = __id;
@synthesize duration = _duration;
@synthesize attributionText = _attributionText;
@synthesize createdBy = _createdBy;
@synthesize description = _description;
@synthesize createdAt = _createdAt;
@synthesize voteWeightedAverage = _voteWeightedAverage;
@synthesize voteAverage = _voteAverage;
@synthesize word = _word;
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
       word: (NSString*) word
       {
          _attributionUrl = attributionUrl;
          _commentCount = commentCount;
          _voteCount = voteCount;
          _fileUrl = fileUrl;
          _audioType = audioType;
          __id = _id;
          _duration = duration;
          _attributionText = attributionText;
          _createdBy = createdBy;
          _description = description;
          _createdAt = createdAt;
          _voteWeightedAverage = voteWeightedAverage;
          _voteAverage = voteAverage;
          _word = word;
          return self;
       }

- (id) initWithValues: (NSDictionary*)dict
{
    _attributionUrl = [dict objectForKey:@"attributionUrl"];
    _commentCount = [dict objectForKey:@"commentCount"];
    _voteCount = [dict objectForKey:@"voteCount"];
    _fileUrl = [dict objectForKey:@"fileUrl"];
    _audioType = [dict objectForKey:@"audioType"];
    __id = [dict objectForKey:@"id"];
    _duration = [dict objectForKey:@"duration"];
    _attributionText = [dict objectForKey:@"attributionText"];
    _createdBy = [dict objectForKey:@"createdBy"];
    _description = [dict objectForKey:@"description"];
    id createdAt_dict = [dict objectForKey:@"createdAt"];
    _createdAt = [[NIKDate alloc]initWithValues:createdAt_dict];
    _voteWeightedAverage = [dict objectForKey:@"voteWeightedAverage"];
    _voteAverage = [dict objectForKey:@"voteAverage"];
    _word = [dict objectForKey:@"word"];
    return self;
}

-(NSDictionary*) asDictionary {
    NSMutableDictionary* dict = [[NSMutableDictionary alloc] init];
    if(_attributionUrl != nil) [dict setObject:_attributionUrl forKey:@"attributionUrl"];
    if(_commentCount != nil) [dict setObject:_commentCount forKey:@"commentCount"];
    if(_voteCount != nil) [dict setObject:_voteCount forKey:@"voteCount"];
    if(_fileUrl != nil) [dict setObject:_fileUrl forKey:@"fileUrl"];
    if(_audioType != nil) [dict setObject:_audioType forKey:@"audioType"];
    if(__id != nil) [dict setObject:__id forKey:@"id"];
    if(_duration != nil) [dict setObject:_duration forKey:@"duration"];
    if(_attributionText != nil) [dict setObject:_attributionText forKey:@"attributionText"];
    if(_createdBy != nil) [dict setObject:_createdBy forKey:@"createdBy"];
    if(_description != nil) [dict setObject:_description forKey:@"description"];
    if(_createdAt != nil){
        if([_createdAt isKindOfClass:[NSArray class]]){
            NSMutableArray * array = [[NSMutableArray alloc] init];
            for( NIKDate * createdAt in (NSArray*)_createdAt) {
                [array addObject:[(NIKSwaggerObject*)createdAt asDictionary]];
            }
            [dict setObject:array forKey:@"createdAt"];
        }
        else if(_createdAt && [_createdAt isKindOfClass:[NIKDate class]]) {
            NSString * dateString = [(NIKDate*)_createdAt toString];
            if(dateString){
                [dict setObject:dateString forKey:@"createdAt"];   
            }
        }
    }
    else {
    if(_createdAt != nil) [dict setObject:[(NIKSwaggerObject*)_createdAt asDictionary]forKey:@"createdAt"];
    }
    if(_voteWeightedAverage != nil) [dict setObject:_voteWeightedAverage forKey:@"voteWeightedAverage"];
    if(_voteAverage != nil) [dict setObject:_voteAverage forKey:@"voteAverage"];
    if(_word != nil) [dict setObject:_word forKey:@"word"];
    NSDictionary* output = [dict copy];
    return output;
}

@end

