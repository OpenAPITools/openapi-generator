#import <Foundation/Foundation.h>
#import "NIKSwaggerObject.h"

@interface NIKSimpleExample : NIKSwaggerObject {
@private
    NSNumber* __id; //NSNumber
    NSString* _text; //NSString
    NSString* _title; //NSString
    NSString* _url; //NSString
    }



@property(nonatomic) NSNumber* _id;
@property(nonatomic) NSString* text;
@property(nonatomic) NSString* title;
@property(nonatomic) NSString* url;
- (id) _id: (NSNumber*) _id
     text: (NSString*) text
     title: (NSString*) title
     url: (NSString*) url;

- (id) initWithValues: (NSDictionary*)dict;
- (NSDictionary*) asDictionary;


@end

