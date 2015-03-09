#import "SWGDate.h"
#import "SWGDefinition.h"

@implementation SWGDefinition

-(id)extendedText: (NSString*) extendedText
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
    partOfSpeech: (NSString*) partOfSpeech
    
{
    _extendedText = extendedText;
    _text = text;
    _sourceDictionary = sourceDictionary;
    _citations = citations;
    _labels = labels;
    _score = score;
    _exampleUses = exampleUses;
    _attributionUrl = attributionUrl;
    _seqString = seqString;
    _attributionText = attributionText;
    _relatedWords = relatedWords;
    _sequence = sequence;
    _word = word;
    _notes = notes;
    _textProns = textProns;
    _partOfSpeech = partOfSpeech;
    

    return self;
}

-(id) initWithValues:(NSDictionary*)dict
{
    self = [super init];
    if(self) {
        _extendedText = dict[@"extendedText"];
        
        _text = dict[@"text"];
        
        _sourceDictionary = dict[@"sourceDictionary"];
        
        
        
        id citations_dict = dict[@"citations"];
        
        if([citations_dict isKindOfClass:[NSArray class]]) {
            NSMutableArray * objs = [[NSMutableArray alloc] initWithCapacity:[(NSArray*)citations_dict count]];
            if([(NSArray*)citations_dict count] > 0) {
                for (NSDictionary* dict in (NSArray*)citations_dict) {
                    SWGCitation* d = [[SWGCitation alloc] initWithValues:dict];
                    [objs addObject:d];
                }
                _citations = [[NSArray alloc] initWithArray:objs];
            }
            else
                _citations = [[NSArray alloc] init];
        }
        else {
            _citations = [[NSArray alloc] init];
        }
        
        
        
        
        id labels_dict = dict[@"labels"];
        
        if([labels_dict isKindOfClass:[NSArray class]]) {
            NSMutableArray * objs = [[NSMutableArray alloc] initWithCapacity:[(NSArray*)labels_dict count]];
            if([(NSArray*)labels_dict count] > 0) {
                for (NSDictionary* dict in (NSArray*)labels_dict) {
                    SWGLabel* d = [[SWGLabel alloc] initWithValues:dict];
                    [objs addObject:d];
                }
                _labels = [[NSArray alloc] initWithArray:objs];
            }
            else
                _labels = [[NSArray alloc] init];
        }
        else {
            _labels = [[NSArray alloc] init];
        }
        
        
        _score = dict[@"score"];
        
        
        
        id exampleUses_dict = dict[@"exampleUses"];
        
        if([exampleUses_dict isKindOfClass:[NSArray class]]) {
            NSMutableArray * objs = [[NSMutableArray alloc] initWithCapacity:[(NSArray*)exampleUses_dict count]];
            if([(NSArray*)exampleUses_dict count] > 0) {
                for (NSDictionary* dict in (NSArray*)exampleUses_dict) {
                    SWGExampleUsage* d = [[SWGExampleUsage alloc] initWithValues:dict];
                    [objs addObject:d];
                }
                _exampleUses = [[NSArray alloc] initWithArray:objs];
            }
            else
                _exampleUses = [[NSArray alloc] init];
        }
        else {
            _exampleUses = [[NSArray alloc] init];
        }
        
        
        _attributionUrl = dict[@"attributionUrl"];
        
        _seqString = dict[@"seqString"];
        
        _attributionText = dict[@"attributionText"];
        
        
        
        id relatedWords_dict = dict[@"relatedWords"];
        
        if([relatedWords_dict isKindOfClass:[NSArray class]]) {
            NSMutableArray * objs = [[NSMutableArray alloc] initWithCapacity:[(NSArray*)relatedWords_dict count]];
            if([(NSArray*)relatedWords_dict count] > 0) {
                for (NSDictionary* dict in (NSArray*)relatedWords_dict) {
                    SWGRelated* d = [[SWGRelated alloc] initWithValues:dict];
                    [objs addObject:d];
                }
                _relatedWords = [[NSArray alloc] initWithArray:objs];
            }
            else
                _relatedWords = [[NSArray alloc] init];
        }
        else {
            _relatedWords = [[NSArray alloc] init];
        }
        
        
        _sequence = dict[@"sequence"];
        
        _word = dict[@"word"];
        
        
        
        id notes_dict = dict[@"notes"];
        
        if([notes_dict isKindOfClass:[NSArray class]]) {
            NSMutableArray * objs = [[NSMutableArray alloc] initWithCapacity:[(NSArray*)notes_dict count]];
            if([(NSArray*)notes_dict count] > 0) {
                for (NSDictionary* dict in (NSArray*)notes_dict) {
                    SWGNote* d = [[SWGNote alloc] initWithValues:dict];
                    [objs addObject:d];
                }
                _notes = [[NSArray alloc] initWithArray:objs];
            }
            else
                _notes = [[NSArray alloc] init];
        }
        else {
            _notes = [[NSArray alloc] init];
        }
        
        
        
        
        id textProns_dict = dict[@"textProns"];
        
        if([textProns_dict isKindOfClass:[NSArray class]]) {
            NSMutableArray * objs = [[NSMutableArray alloc] initWithCapacity:[(NSArray*)textProns_dict count]];
            if([(NSArray*)textProns_dict count] > 0) {
                for (NSDictionary* dict in (NSArray*)textProns_dict) {
                    SWGTextPron* d = [[SWGTextPron alloc] initWithValues:dict];
                    [objs addObject:d];
                }
                _textProns = [[NSArray alloc] initWithArray:objs];
            }
            else
                _textProns = [[NSArray alloc] init];
        }
        else {
            _textProns = [[NSArray alloc] init];
        }
        
        
        _partOfSpeech = dict[@"partOfSpeech"];
        
        
    }
    return self;
}

-(NSDictionary*) asDictionary {
    NSMutableDictionary* dict = [[NSMutableDictionary alloc] init];
    
    
            if(_extendedText != nil) dict[@"extendedText"] = _extendedText ;
        
    
    
            if(_text != nil) dict[@"text"] = _text ;
        
    
    
            if(_sourceDictionary != nil) dict[@"sourceDictionary"] = _sourceDictionary ;
        
    
    
    if(_citations != nil){
        if([_citations isKindOfClass:[NSArray class]]){
            NSMutableArray * array = [[NSMutableArray alloc] init];
            for( SWGCitation *citations in (NSArray*)_citations) {
                [array addObject:[(SWGObject*)citations asDictionary]];
            }
            dict[@"citations"] = array;
        }
        else if(_citations && [_citations isKindOfClass:[SWGDate class]]) {
            NSString * dateString = [(SWGDate*)_citations toString];
            if(dateString){
                dict[@"citations"] = dateString;
            }
        }
        else {
        
            if(_citations != nil) dict[@"citations"] = [(SWGObject*)_citations asDictionary];
        
        }
    }
    
    
    
    if(_labels != nil){
        if([_labels isKindOfClass:[NSArray class]]){
            NSMutableArray * array = [[NSMutableArray alloc] init];
            for( SWGLabel *labels in (NSArray*)_labels) {
                [array addObject:[(SWGObject*)labels asDictionary]];
            }
            dict[@"labels"] = array;
        }
        else if(_labels && [_labels isKindOfClass:[SWGDate class]]) {
            NSString * dateString = [(SWGDate*)_labels toString];
            if(dateString){
                dict[@"labels"] = dateString;
            }
        }
        else {
        
            if(_labels != nil) dict[@"labels"] = [(SWGObject*)_labels asDictionary];
        
        }
    }
    
    
    
            if(_score != nil) dict[@"score"] = _score ;
        
    
    
    if(_exampleUses != nil){
        if([_exampleUses isKindOfClass:[NSArray class]]){
            NSMutableArray * array = [[NSMutableArray alloc] init];
            for( SWGExampleUsage *exampleUses in (NSArray*)_exampleUses) {
                [array addObject:[(SWGObject*)exampleUses asDictionary]];
            }
            dict[@"exampleUses"] = array;
        }
        else if(_exampleUses && [_exampleUses isKindOfClass:[SWGDate class]]) {
            NSString * dateString = [(SWGDate*)_exampleUses toString];
            if(dateString){
                dict[@"exampleUses"] = dateString;
            }
        }
        else {
        
            if(_exampleUses != nil) dict[@"exampleUses"] = [(SWGObject*)_exampleUses asDictionary];
        
        }
    }
    
    
    
            if(_attributionUrl != nil) dict[@"attributionUrl"] = _attributionUrl ;
        
    
    
            if(_seqString != nil) dict[@"seqString"] = _seqString ;
        
    
    
            if(_attributionText != nil) dict[@"attributionText"] = _attributionText ;
        
    
    
    if(_relatedWords != nil){
        if([_relatedWords isKindOfClass:[NSArray class]]){
            NSMutableArray * array = [[NSMutableArray alloc] init];
            for( SWGRelated *relatedWords in (NSArray*)_relatedWords) {
                [array addObject:[(SWGObject*)relatedWords asDictionary]];
            }
            dict[@"relatedWords"] = array;
        }
        else if(_relatedWords && [_relatedWords isKindOfClass:[SWGDate class]]) {
            NSString * dateString = [(SWGDate*)_relatedWords toString];
            if(dateString){
                dict[@"relatedWords"] = dateString;
            }
        }
        else {
        
            if(_relatedWords != nil) dict[@"relatedWords"] = [(SWGObject*)_relatedWords asDictionary];
        
        }
    }
    
    
    
            if(_sequence != nil) dict[@"sequence"] = _sequence ;
        
    
    
            if(_word != nil) dict[@"word"] = _word ;
        
    
    
    if(_notes != nil){
        if([_notes isKindOfClass:[NSArray class]]){
            NSMutableArray * array = [[NSMutableArray alloc] init];
            for( SWGNote *notes in (NSArray*)_notes) {
                [array addObject:[(SWGObject*)notes asDictionary]];
            }
            dict[@"notes"] = array;
        }
        else if(_notes && [_notes isKindOfClass:[SWGDate class]]) {
            NSString * dateString = [(SWGDate*)_notes toString];
            if(dateString){
                dict[@"notes"] = dateString;
            }
        }
        else {
        
            if(_notes != nil) dict[@"notes"] = [(SWGObject*)_notes asDictionary];
        
        }
    }
    
    
    
    if(_textProns != nil){
        if([_textProns isKindOfClass:[NSArray class]]){
            NSMutableArray * array = [[NSMutableArray alloc] init];
            for( SWGTextPron *textProns in (NSArray*)_textProns) {
                [array addObject:[(SWGObject*)textProns asDictionary]];
            }
            dict[@"textProns"] = array;
        }
        else if(_textProns && [_textProns isKindOfClass:[SWGDate class]]) {
            NSString * dateString = [(SWGDate*)_textProns toString];
            if(dateString){
                dict[@"textProns"] = dateString;
            }
        }
        else {
        
            if(_textProns != nil) dict[@"textProns"] = [(SWGObject*)_textProns asDictionary];
        
        }
    }
    
    
    
            if(_partOfSpeech != nil) dict[@"partOfSpeech"] = _partOfSpeech ;
        
    

    NSDictionary* output = [dict copy];
    return output;
}

@end
