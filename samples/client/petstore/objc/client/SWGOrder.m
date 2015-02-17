#import "SWGDate.h"
#import "SWGOrder.h"

@implementation SWGOrder

-(id)_id: (NSNumber*) _id
    petId: (NSNumber*) petId
    quantity: (NSNumber*) quantity
    shipDate: (SWGDate*) shipDate
    status: (NSString*) status
    complete: (NSNumber*) complete
    
{
    __id = _id;
    _petId = petId;
    _quantity = quantity;
    _shipDate = shipDate;
    _status = status;
    _complete = complete;
    

    return self;
}

-(id) initWithValues:(NSDictionary*)dict
{
    self = [super init];
    if(self) {
        __id = dict[@"id"];
        
        _petId = dict[@"petId"];
        
        _quantity = dict[@"quantity"];
        
        
        
        id shipDate_dict = dict[@"shipDate"];
        
        if(shipDate_dict != nil)
            _shipDate = [[SWGDate  alloc]initWithValues:shipDate_dict];
        
        
        _status = dict[@"status"];
        
        _complete = dict[@"complete"];
        
        
    }
    return self;
}

-(NSDictionary*) asDictionary {
    NSMutableDictionary* dict = [[NSMutableDictionary alloc] init];
    
    
            if(__id != nil) dict[@"id"] = __id ;
        
    
    
            if(_petId != nil) dict[@"petId"] = _petId ;
        
    
    
            if(_quantity != nil) dict[@"quantity"] = _quantity ;
        
    
    
    if(_shipDate != nil){
        if([_shipDate isKindOfClass:[NSArray class]]){
            NSMutableArray * array = [[NSMutableArray alloc] init];
            for( SWGDate *shipDate in (NSArray*)_shipDate) {
                [array addObject:[(SWGObject*)shipDate asDictionary]];
            }
            dict[@"shipDate"] = array;
        }
        else if(_shipDate && [_shipDate isKindOfClass:[SWGDate class]]) {
            NSString * dateString = [(SWGDate*)_shipDate toString];
            if(dateString){
                dict[@"shipDate"] = dateString;
            }
        }
        else {
        
            if(_shipDate != nil) dict[@"shipDate"] = [(SWGObject*)_shipDate asDictionary];
        
        }
    }
    
    
    
            if(_status != nil) dict[@"status"] = _status ;
        
    
    
            if(_complete != nil) dict[@"complete"] = _complete ;
        
    

    NSDictionary* output = [dict copy];
    return output;
}

@end
