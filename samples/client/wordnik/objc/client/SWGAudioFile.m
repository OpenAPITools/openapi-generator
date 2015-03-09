#import "SWGDate.h"
#import "SWGAudioFile.h"

@implementation SWGAudioFile

-(id)attributionUrl: (NSString*) attributionUrl
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
    __description = _description;
    _createdAt = createdAt;
    _voteWeightedAverage = voteWeightedAverage;
    _voteAverage = voteAverage;
    _word = word;
    

    return self;
}

-(id) initWithValues:(NSDictionary*)dict
{
    self = [super init];
    if(self) {
        _attributionUrl = dict[@"attributionUrl"];
        
        _commentCount = dict[@"commentCount"];
        
        _voteCount = dict[@"voteCount"];
        
        _fileUrl = dict[@"fileUrl"];
        
        _audioType = dict[@"audioType"];
        
        __id = dict[@"id"];
        
        _duration = dict[@"duration"];
        
        _attributionText = dict[@"attributionText"];
        
        _createdBy = dict[@"createdBy"];
        
        __description = dict[@"description"];
        
        
        
        id createdAt_dict = dict[@"createdAt"];
        
        if(createdAt_dict != nil)
            _createdAt = [[SWGDate  alloc]initWithValues:createdAt_dict];
        
        
        _voteWeightedAverage = dict[@"voteWeightedAverage"];
        
        _voteAverage = dict[@"voteAverage"];
        
        _word = dict[@"word"];
        
        
    }
    return self;
}

-(NSDictionary*) asDictionary {
    NSMutableDictionary* dict = [[NSMutableDictionary alloc] init];
    
    
            if(_attributionUrl != nil) dict[@"attributionUrl"] = _attributionUrl ;
        
    
    
            if(_commentCount != nil) dict[@"commentCount"] = _commentCount ;
        
    
    
            if(_voteCount != nil) dict[@"voteCount"] = _voteCount ;
        
    
    
            if(_fileUrl != nil) dict[@"fileUrl"] = _fileUrl ;
        
    
    
            if(_audioType != nil) dict[@"audioType"] = _audioType ;
        
    
    
            if(__id != nil) dict[@"id"] = __id ;
        
    
    
            if(_duration != nil) dict[@"duration"] = _duration ;
        
    
    
            if(_attributionText != nil) dict[@"attributionText"] = _attributionText ;
        
    
    
            if(_createdBy != nil) dict[@"createdBy"] = _createdBy ;
        
    
    
            if(__description != nil) dict[@"description"] = __description ;
        
    
    
    if(_createdAt != nil){
        if([_createdAt isKindOfClass:[NSArray class]]){
            NSMutableArray * array = [[NSMutableArray alloc] init];
            for( SWGDate *createdAt in (NSArray*)_createdAt) {
                [array addObject:[(SWGObject*)createdAt asDictionary]];
            }
            dict[@"createdAt"] = array;
        }
        else if(_createdAt && [_createdAt isKindOfClass:[SWGDate class]]) {
            NSString * dateString = [(SWGDate*)_createdAt toString];
            if(dateString){
                dict[@"createdAt"] = dateString;
            }
        }
        else {
        
            if(_createdAt != nil) dict[@"createdAt"] = [(SWGObject*)_createdAt asDictionary];
        
        }
    }
    
    
    
            if(_voteWeightedAverage != nil) dict[@"voteWeightedAverage"] = _voteWeightedAverage ;
        
    
    
            if(_voteAverage != nil) dict[@"voteAverage"] = _voteAverage ;
        
    
    
            if(_word != nil) dict[@"word"] = _word ;
        
    

    NSDictionary* output = [dict copy];
    return output;
}

@end
