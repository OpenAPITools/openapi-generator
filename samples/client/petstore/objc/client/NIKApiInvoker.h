#import <Foundation/Foundation.h>

@interface NIKApiInvoker : NSObject {
    
@private
    NSOperationQueue *_queue;
    NSMutableDictionary * _defaultHeaders;
}
@property(nonatomic, readonly) NSOperationQueue* queue;
@property(nonatomic, readonly) NSMutableDictionary * defaultHeaders;
@property(nonatomic, assign) NSURLRequestCachePolicy cachePolicy;

+ (NIKApiInvoker*)sharedInstance;

- (void)updateLoadCountWithDelta:(NSInteger)countDelta;
- (void)startLoad;
- (void)stopLoad;


-(void) addHeader:(NSString*)value forKey:(NSString*)key;

-(NSString*) escapeString:(NSString*) string;

-(void) dictionary: (NSString*) path
            method: (NSString*) method
       queryParams: (NSDictionary*) queryParams
              body: (id)body
      headerParams: (NSDictionary*) headerParams
       contentType: contentType
   completionBlock: (void (^)(NSDictionary*, NSError *))completionBlock;

-(void) stringWithCompletionBlock: (NSString*) path
                           method: (NSString*) method
                      queryParams: (NSDictionary*) queryParams
                             body: (id)body
                     headerParams: (NSDictionary*) headerParams
                      contentType: contentType
                  completionBlock: (void (^)(NSString*, NSError *))completionBlock;

@end
