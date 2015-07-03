#import <Foundation/Foundation.h>

@interface SWGFile : NSObject

/**
 * File name
 */
@property(nonatomic, readonly) NSString* name;

/**
 * File mimeType, for `multipart/form` post
 */
@property(nonatomic, readonly) NSString* mimeType;

/**
 * File data
 */
@property(nonatomic, readonly) NSData* data;

/**
 * File field parameter name, for `multipart/form` post
 */
@property(nonatomic) NSString* paramName;

/**
 * File path
 */
@property(nonatomic, readonly) NSString *path;

/**
 * Initialize with `filename`, `mimeType`, `filedata`.
 */
- (id) initWithNameData: (NSString*) filename
               mimeType: (NSString*) mimeType
                   data: (NSData*) data;

/**
 * Initialize with `filepath`, `filedata`, `filename`
 * and write file data into file path.
 */
- (id) initWithPath: (NSString *) filepath
               data: (NSData *) data
               name: (NSString *) filename;
    
@end
