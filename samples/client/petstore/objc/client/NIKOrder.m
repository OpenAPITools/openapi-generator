#import "NIKDate.h"
#import "NIKOrder.h"

@implementation NIKOrder

@synthesize _id = __id;
@synthesize petId = _petId;
@synthesize status = _status;
@synthesize quantity = _quantity;
@synthesize shipDate = _shipDate;
- (id) _id: (NSNumber*) _id
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

- (id) initWithValues: (NSDictionary*)dict
{
    __id = [dict objectForKey:@"id"];
    _petId = [dict objectForKey:@"petId"];
    _status = [dict objectForKey:@"status"];
    _quantity = [dict objectForKey:@"quantity"];
    id shipDate_dict = [dict objectForKey:@"shipDate"];
    _shipDate = [[NIKDate alloc]initWithValues:shipDate_dict];
    return self;
}

-(NSDictionary*) asDictionary {
    NSMutableDictionary* dict = [[NSMutableDictionary alloc] init];
    if(__id != nil) [dict setObject:__id forKey:@"id"];
    if(_petId != nil) [dict setObject:_petId forKey:@"petId"];
    if(_status != nil) [dict setObject:_status forKey:@"status"];
    if(_quantity != nil) [dict setObject:_quantity forKey:@"quantity"];
    if(_shipDate != nil){
        if([_shipDate isKindOfClass:[NSArray class]]){
            NSMutableArray * array = [[NSMutableArray alloc] init];
            for( NIKDate * shipDate in (NSArray*)_shipDate) {
                [array addObject:[(NIKSwaggerObject*)shipDate asDictionary]];
            }
            [dict setObject:array forKey:@"shipDate"];
        }
        else if(_shipDate && [_shipDate isKindOfClass:[NIKDate class]]) {
            NSString * dateString = [(NIKDate*)_shipDate toString];
            if(dateString){
                [dict setObject:dateString forKey:@"shipDate"];   
            }
        }
    }
    else {
    if(_shipDate != nil) [dict setObject:[(NIKSwaggerObject*)_shipDate asDictionary]forKey:@"shipDate"];
    }
    NSDictionary* output = [dict copy];
    return output;
}

@end

