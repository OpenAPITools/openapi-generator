#import "NIKDate.h"
#import "NIKOrder.h"

@implementation NIKOrder

-(id)_id: (NSNumber*) _id
    petId: (NSNumber*) petId
    status: (NSString*) status
    quantity: (NSNumber*) quantity
    shipDate: (NIKDate*) shipDate
{
  __id = _id;
  _petId = petId;
  _status = status;
  _quantity = quantity;
  _shipDate = shipDate;
  return self;
}

-(id) initWithValues:(NSDictionary*)dict
{
    self = [super init];
    if(self) {
        __id = dict[@"id"]; 
        _petId = dict[@"petId"]; 
        _status = dict[@"status"]; 
        _quantity = dict[@"quantity"]; 
        id shipDate_dict = dict[@"shipDate"];
        _shipDate = [[NIKDate alloc]initWithValues:shipDate_dict];
        

    }
    return self;
}

-(NSDictionary*) asDictionary {
    NSMutableDictionary* dict = [[NSMutableDictionary alloc] init];
    if(__id != nil) dict[@"id"] = __id ;
    if(_petId != nil) dict[@"petId"] = _petId ;
    if(_status != nil) dict[@"status"] = _status ;
    if(_quantity != nil) dict[@"quantity"] = _quantity ;
    if(_shipDate != nil){
        if([_shipDate isKindOfClass:[NSArray class]]){
            NSMutableArray * array = [[NSMutableArray alloc] init];
            for( NIKDate *shipDate in (NSArray*)_shipDate) {
                [array addObject:[(NIKSwaggerObject*)shipDate asDictionary]];
            }
            dict[@"shipDate"] = array;
        }
        else if(_shipDate && [_shipDate isKindOfClass:[NIKDate class]]) {
            NSString * dateString = [(NIKDate*)_shipDate toString];
            if(dateString){
                dict[@"shipDate"] = dateString;
            }
        }
    }
    else {
    if(_shipDate != nil) dict[@"shipDate"] = [(NIKSwaggerObject*)_shipDate asDictionary];
    }
    NSDictionary* output = [dict copy];
    return output;
}

@end

