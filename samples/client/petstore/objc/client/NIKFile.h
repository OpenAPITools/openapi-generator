#import <Foundation/Foundation.h>

@interface NIKFile : NSObject {
@private
    NSString* name;
    NSString* mimeType;
    NSData* data;
}
@property(nonatomic, readonly) NSString* name;
@property(nonatomic, readonly) NSString* mimeType;
@property(nonatomic, readonly) NSData* data;

- (id) initWithNameData: (NSString*) filename
               mimeType: (NSString*) mimeType
                   data: (NSData*) data;
    
    @end