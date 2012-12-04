#import <Foundation/Foundation.h>
#import "NIKSwaggerObject.h"

@interface NIKSimpleExample : NIKSwaggerObject {
@private
    NSNumber* __id; //NSNumber
    NSString* _title; //NSString
    NSString* _text; //NSString
    NSString* _url; //NSString
    }



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

