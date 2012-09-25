#import <Foundation/Foundation.h>

@interface NIKApiInvoker : NSObject {
    
@private
    NSOperationQueue *_queue;
    NSMutableDictionary * _defaultHeaders;
}
@property(nonatomic, readonly) NSOperationQueue* queue;
@property(nonatomic, readonly) NSMutableDictionary * defaultHeaders;


-(void) addHeader:(NSString*) value
           forKey:(NSString*)key;

-(NSString*) escapeString:(NSString*) string;

-(id) dictionaryWithCompletionBlock:(NSString*) path
                             method:(NSString*) method
                        queryParams:(NSDictionary*) queryParams
                               body:(id)body
                       headerParams:(NSDictionary*) headerParams
                  completionHandler:(void (^)(NSDictionary*, NSError *))completionBlock;

-(id) stringWithCompletionBlock:(NSString*) path
                         method:(NSString*) method
                    queryParams:(NSDictionary*) queryParams
                           body:(id)body
                   headerParams:(NSDictionary*) headerParams
              completionHandler:(void (^)(NSString*, NSError *))completionBlock;

@end
