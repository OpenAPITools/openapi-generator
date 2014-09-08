#import <Foundation/Foundation.h>
#import "SWGObject.h"


@interface SWGSimpleExample : SWGObject

@property(nonatomic) NSNumber* _id;  

@property(nonatomic) NSString* title;  

@property(nonatomic) NSString* text;  

@property(nonatomic) NSString* url;  

- (id) _id: (NSNumber*) _id
     title: (NSString*) title
     text: (NSString*) text
     url: (NSString*) url;

- (id) initWithValues: (NSDictionary*)dict;
- (NSDictionary*) asDictionary;


@end

