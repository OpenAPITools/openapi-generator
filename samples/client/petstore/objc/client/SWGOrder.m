#import "SWGDate.h"
#import "SWGOrder.h"

@implementation SWGOrder

-(id)_id: (NSNumber*) _id
    petId: (NSNumber*) petId
    quantity: (NSNumber*) quantity
    shipDate: (SWGDate*) shipDate
    status: (NSString*) status
    complete: (NSNumber*) complete
    keyValuePairs: (SWGMap*) keyValuePairs { 
    
    __id = _id;
    _petId = petId;
    _quantity = quantity;
    _shipDate = shipDate;
    _status = status;
    _complete = complete;
    _keyValuePairs = keyValuePairs;
    
    return self;
}

-(id) initWithValues:(NSDictionary*)dict
{
    self = [super init];
    if(self) {
        __id = dict[@"id"];
        _petId = dict[@"petId"];
        _quantity = dict[@"quantity"];
        
        _status = dict[@"status"];
        _complete = dict[@"complete"];
        
        
    }
    return self;
}

-(NSDictionary*) asDictionary {
    NSMutableDictionary* dict = [[NSMutableDictionary alloc] init];
    if(__id != nil)
        dict[@"id"] = [(SWGObject*)__id asDictionary];
    if(_petId != nil)
        dict[@"petId"] = [(SWGObject*)_petId asDictionary];
    if(_quantity != nil)
        dict[@"quantity"] = [(SWGObject*)_quantity asDictionary];
    if(_status != nil)
        dict[@"status"] = [(SWGObject*)_status asDictionary];
    if(_complete != nil)
        dict[@"complete"] = [(SWGObject*)_complete asDictionary];
    
    NSDictionary* output = [dict copy];
    return output;
}

@end
