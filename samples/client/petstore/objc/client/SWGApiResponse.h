#import <Foundation/Foundation.h>
#import "SWGObject.h"


@protocol SWGApiResponse
@end
  
@interface SWGApiResponse : SWGObject


@property(nonatomic) NSNumber* code;

@property(nonatomic) NSString* type;

@property(nonatomic) NSString* message;

@end
