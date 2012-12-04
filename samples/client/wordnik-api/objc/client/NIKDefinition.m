#import "NIKDate.h"
#import "NIKDefinition.h"

@implementation NIKDefinition

@synthesize extendedText = _extendedText;
@synthesize text = _text;
@synthesize sourceDictionary = _sourceDictionary;
@synthesize citations = _citations;
@synthesize labels = _labels;
@synthesize score = _score;
@synthesize exampleUses = _exampleUses;
@synthesize attributionUrl = _attributionUrl;
@synthesize seqString = _seqString;
@synthesize attributionText = _attributionText;
@synthesize relatedWords = _relatedWords;
@synthesize sequence = _sequence;
@synthesize word = _word;
@synthesize notes = _notes;
@synthesize textProns = _textProns;
@synthesize partOfSpeech = _partOfSpeech;
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

- (id) initWithValues: (NSDictionary*)dict
{
    _extendedText = [dict objectForKey:@"extendedText"];
    _text = [dict objectForKey:@"text"];
    _sourceDictionary = [dict objectForKey:@"sourceDictionary"];
    id citations_dict = [dict objectForKey:@"citations"];
    if([citations_dict isKindOfClass:[NSArray class]]) {
        if([(NSArray*)citations_dict count] > 0) {
            NSMutableArray * objs = [[NSMutableArray alloc] initWithCapacity:[(NSArray*)citations_dict count]];
            for (NSDictionary* dict in (NSArray*)citations_dict) {
                NIKCitation* d = [[NIKCitation alloc]initWithValues:dict];
                [objs addObject:d];
            }
            _citations = [[NSArray alloc] initWithArray:objs];
        }
    }
    id labels_dict = [dict objectForKey:@"labels"];
    if([labels_dict isKindOfClass:[NSArray class]]) {
        if([(NSArray*)labels_dict count] > 0) {
            NSMutableArray * objs = [[NSMutableArray alloc] initWithCapacity:[(NSArray*)labels_dict count]];
            for (NSDictionary* dict in (NSArray*)labels_dict) {
                NIKLabel* d = [[NIKLabel alloc]initWithValues:dict];
                [objs addObject:d];
            }
            _labels = [[NSArray alloc] initWithArray:objs];
        }
    }
    _score = [dict objectForKey:@"score"];
    id exampleUses_dict = [dict objectForKey:@"exampleUses"];
    if([exampleUses_dict isKindOfClass:[NSArray class]]) {
        if([(NSArray*)exampleUses_dict count] > 0) {
            NSMutableArray * objs = [[NSMutableArray alloc] initWithCapacity:[(NSArray*)exampleUses_dict count]];
            for (NSDictionary* dict in (NSArray*)exampleUses_dict) {
                NIKExampleUsage* d = [[NIKExampleUsage alloc]initWithValues:dict];
                [objs addObject:d];
            }
            _exampleUses = [[NSArray alloc] initWithArray:objs];
        }
    }
    _attributionUrl = [dict objectForKey:@"attributionUrl"];
    _seqString = [dict objectForKey:@"seqString"];
    _attributionText = [dict objectForKey:@"attributionText"];
    id relatedWords_dict = [dict objectForKey:@"relatedWords"];
    if([relatedWords_dict isKindOfClass:[NSArray class]]) {
        if([(NSArray*)relatedWords_dict count] > 0) {
            NSMutableArray * objs = [[NSMutableArray alloc] initWithCapacity:[(NSArray*)relatedWords_dict count]];
            for (NSDictionary* dict in (NSArray*)relatedWords_dict) {
                NIKRelated* d = [[NIKRelated alloc]initWithValues:dict];
                [objs addObject:d];
            }
            _relatedWords = [[NSArray alloc] initWithArray:objs];
        }
    }
    _sequence = [dict objectForKey:@"sequence"];
    _word = [dict objectForKey:@"word"];
    id notes_dict = [dict objectForKey:@"notes"];
    if([notes_dict isKindOfClass:[NSArray class]]) {
        if([(NSArray*)notes_dict count] > 0) {
            NSMutableArray * objs = [[NSMutableArray alloc] initWithCapacity:[(NSArray*)notes_dict count]];
            for (NSDictionary* dict in (NSArray*)notes_dict) {
                NIKNote* d = [[NIKNote alloc]initWithValues:dict];
                [objs addObject:d];
            }
            _notes = [[NSArray alloc] initWithArray:objs];
        }
    }
    id textProns_dict = [dict objectForKey:@"textProns"];
    if([textProns_dict isKindOfClass:[NSArray class]]) {
        if([(NSArray*)textProns_dict count] > 0) {
            NSMutableArray * objs = [[NSMutableArray alloc] initWithCapacity:[(NSArray*)textProns_dict count]];
            for (NSDictionary* dict in (NSArray*)textProns_dict) {
                NIKTextPron* d = [[NIKTextPron alloc]initWithValues:dict];
                [objs addObject:d];
            }
            _textProns = [[NSArray alloc] initWithArray:objs];
        }
    }
    _partOfSpeech = [dict objectForKey:@"partOfSpeech"];
    return self;
}

-(NSDictionary*) asDictionary {
    NSMutableDictionary* dict = [[NSMutableDictionary alloc] init];
    if(_extendedText != nil) [dict setObject:_extendedText forKey:@"extendedText"];
    if(_text != nil) [dict setObject:_text forKey:@"text"];
    if(_sourceDictionary != nil) [dict setObject:_sourceDictionary forKey:@"sourceDictionary"];
    if(_citations != nil){
        if([_citations isKindOfClass:[NSArray class]]){
            NSMutableArray * array = [[NSMutableArray alloc] init];
            for( NIKCitation * citations in (NSArray*)_citations) {
                [array addObject:[(NIKSwaggerObject*)citations asDictionary]];
            }
            [dict setObject:array forKey:@"citations"];
        }
        else if(_citations && [_citations isKindOfClass:[NIKDate class]]) {
            NSString * dateString = [(NIKDate*)_citations toString];
            if(dateString){
                [dict setObject:dateString forKey:@"citations"];   
            }
        }
    }
    else {
    if(_citations != nil) [dict setObject:[(NIKSwaggerObject*)_citations asDictionary]forKey:@"citations"];
    }
    if(_labels != nil){
        if([_labels isKindOfClass:[NSArray class]]){
            NSMutableArray * array = [[NSMutableArray alloc] init];
            for( NIKLabel * labels in (NSArray*)_labels) {
                [array addObject:[(NIKSwaggerObject*)labels asDictionary]];
            }
            [dict setObject:array forKey:@"labels"];
        }
        else if(_labels && [_labels isKindOfClass:[NIKDate class]]) {
            NSString * dateString = [(NIKDate*)_labels toString];
            if(dateString){
                [dict setObject:dateString forKey:@"labels"];   
            }
        }
    }
    else {
    if(_labels != nil) [dict setObject:[(NIKSwaggerObject*)_labels asDictionary]forKey:@"labels"];
    }
    if(_score != nil) [dict setObject:_score forKey:@"score"];
    if(_exampleUses != nil){
        if([_exampleUses isKindOfClass:[NSArray class]]){
            NSMutableArray * array = [[NSMutableArray alloc] init];
            for( NIKExampleUsage * exampleUses in (NSArray*)_exampleUses) {
                [array addObject:[(NIKSwaggerObject*)exampleUses asDictionary]];
            }
            [dict setObject:array forKey:@"exampleUses"];
        }
        else if(_exampleUses && [_exampleUses isKindOfClass:[NIKDate class]]) {
            NSString * dateString = [(NIKDate*)_exampleUses toString];
            if(dateString){
                [dict setObject:dateString forKey:@"exampleUses"];   
            }
        }
    }
    else {
    if(_exampleUses != nil) [dict setObject:[(NIKSwaggerObject*)_exampleUses asDictionary]forKey:@"exampleUses"];
    }
    if(_attributionUrl != nil) [dict setObject:_attributionUrl forKey:@"attributionUrl"];
    if(_seqString != nil) [dict setObject:_seqString forKey:@"seqString"];
    if(_attributionText != nil) [dict setObject:_attributionText forKey:@"attributionText"];
    if(_relatedWords != nil){
        if([_relatedWords isKindOfClass:[NSArray class]]){
            NSMutableArray * array = [[NSMutableArray alloc] init];
            for( NIKRelated * relatedWords in (NSArray*)_relatedWords) {
                [array addObject:[(NIKSwaggerObject*)relatedWords asDictionary]];
            }
            [dict setObject:array forKey:@"relatedWords"];
        }
        else if(_relatedWords && [_relatedWords isKindOfClass:[NIKDate class]]) {
            NSString * dateString = [(NIKDate*)_relatedWords toString];
            if(dateString){
                [dict setObject:dateString forKey:@"relatedWords"];   
            }
        }
    }
    else {
    if(_relatedWords != nil) [dict setObject:[(NIKSwaggerObject*)_relatedWords asDictionary]forKey:@"relatedWords"];
    }
    if(_sequence != nil) [dict setObject:_sequence forKey:@"sequence"];
    if(_word != nil) [dict setObject:_word forKey:@"word"];
    if(_notes != nil){
        if([_notes isKindOfClass:[NSArray class]]){
            NSMutableArray * array = [[NSMutableArray alloc] init];
            for( NIKNote * notes in (NSArray*)_notes) {
                [array addObject:[(NIKSwaggerObject*)notes asDictionary]];
            }
            [dict setObject:array forKey:@"notes"];
        }
        else if(_notes && [_notes isKindOfClass:[NIKDate class]]) {
            NSString * dateString = [(NIKDate*)_notes toString];
            if(dateString){
                [dict setObject:dateString forKey:@"notes"];   
            }
        }
    }
    else {
    if(_notes != nil) [dict setObject:[(NIKSwaggerObject*)_notes asDictionary]forKey:@"notes"];
    }
    if(_textProns != nil){
        if([_textProns isKindOfClass:[NSArray class]]){
            NSMutableArray * array = [[NSMutableArray alloc] init];
            for( NIKTextPron * textProns in (NSArray*)_textProns) {
                [array addObject:[(NIKSwaggerObject*)textProns asDictionary]];
            }
            [dict setObject:array forKey:@"textProns"];
        }
        else if(_textProns && [_textProns isKindOfClass:[NIKDate class]]) {
            NSString * dateString = [(NIKDate*)_textProns toString];
            if(dateString){
                [dict setObject:dateString forKey:@"textProns"];   
            }
        }
    }
    else {
    if(_textProns != nil) [dict setObject:[(NIKSwaggerObject*)_textProns asDictionary]forKey:@"textProns"];
    }
    if(_partOfSpeech != nil) [dict setObject:_partOfSpeech forKey:@"partOfSpeech"];
    NSDictionary* output = [dict copy];
    return output;
}

@end

