#import <Foundation/Foundation.h>
#import "AFHTTPRequestOperationManager.h"

/**
 * A key for `NSError` user info dictionaries.
 *
 * The corresponding value is the parsed response body for an HTTP error.
 */
extern NSString *const SWGResponseObjectErrorKey;


@interface SWGApiClient : AFHTTPRequestOperationManager

@property(nonatomic, assign) NSURLRequestCachePolicy cachePolicy;
@property(nonatomic, assign) NSTimeInterval timeoutInterval;
@property(nonatomic, assign) BOOL logRequests;
@property(nonatomic, assign) BOOL logCacheHits;
@property(nonatomic, assign) BOOL logServerResponses;
@property(nonatomic, assign) BOOL logJSON;
@property(nonatomic, assign) BOOL logHTTP;
@property(nonatomic, readonly) NSOperationQueue* queue;

/**
 * Get the Api Client instance from pool
 */
+(SWGApiClient *)sharedClientFromPool:(NSString *)baseUrl;

/**
 * Get the operations queue
 */
+(NSOperationQueue*) sharedQueue;

/**
 * Turn on logging
 */
+(void)setLoggingEnabled:(bool) state;

/**
 * Clear Cache
 */
+(void)clearCache;

/**
 * Turn on cache
 */
+(void)setCacheEnabled:(BOOL) enabled;

/**
 * Get the request queue size
 */
+(unsigned long)requestQueueSize;

/**
 * Set the client unreachable
 */
+(void) setOfflineState:(BOOL) state;

/**
 * Get the client reachability
 */
+(AFNetworkReachabilityStatus) getReachabilityStatus;

/**
 * Get the next request id
 */
+(NSNumber*) nextRequestId;

/**
 * Generate request id and add it to the queue
 */
+(NSNumber*) queueRequest;

/**
 * Remove request id from the queue
 */
+(void) cancelRequest:(NSNumber*)requestId;

/**
 * URL encode NSString
 */
+(NSString*) escape:(id)unescaped;

/**
 * Set the client reachability
 */
+(void) setReachabilityChangeBlock:(void(^)(int))changeBlock;

/**
 * Set the client reachability strategy
 */
+(void) configureCacheReachibilityForHost:(NSString*)host;

/**
 * Detect Accept header from accepts NSArray
 */
+(NSString *) selectHeaderAccept:(NSArray *)accepts;

/**
 * Detect Content-Type header from contentTypes NSArray
 */
+(NSString *) selectHeaderContentType:(NSArray *)contentTypes;

/**
 * Set header for request
 */
-(void)setHeaderValue:(NSString*) value
                forKey:(NSString*) forKey;

/**
 * Update header parameters and query parameters for authentication
 */
- (void) updateHeaderParams:(NSDictionary **)headers
                queryParams:(NSDictionary **)querys
           WithAuthSettings:(NSArray *)authSettings;

/**
 * Perform request
 *
 * @discussion Request with non-empty response
 */
-(NSNumber*)  dictionary:(NSString*) path
                  method:(NSString*) method
             queryParams:(NSDictionary*) queryParams
                    body:(id) body
            headerParams:(NSDictionary*) headerParams
            authSettings: (NSArray *) authSettings
      requestContentType:(NSString*) requestContentType
     responseContentType:(NSString*) responseContentType
         completionBlock:(void (^)(NSDictionary*, NSError *))completionBlock;

/**
 * Perform request
 *
 * @discussion Request with empty response
 */
-(NSNumber*)  stringWithCompletionBlock:(NSString*) path
                                 method:(NSString*) method
                            queryParams:(NSDictionary*) queryParams
                                   body:(id) body
                           headerParams:(NSDictionary*) headerParams
                           authSettings: (NSArray *) authSettings
                     requestContentType:(NSString*) requestContentType
                    responseContentType:(NSString*) responseContentType
                        completionBlock:(void (^)(NSString*, NSError *))completionBlock;
@end
