#import <Foundation/Foundation.h>
#import "SWGObject.h"
#import "SWGCategory.h"


@interface SWGRoot : SWGObject

@property(nonatomic) NSNumber* _id;  
@property(nonatomic) NSString* name;  
@property(nonatomic) NSArray* categories;  
- (id) _id: (NSNumber*) _id     
    name: (NSString*) name     
    categories: (NSArray*) categories;
    

- (id) initWithValues: (NSDictionary*)dict;
- (NSDictionary*) asDictionary;

@end
