#import "PromiseKit.h"

@interface NSError (PMK)
- (BOOL)isCancelled;
@end

static dispatch_once_t __PMKDefaultDispatchQueueToken;
static dispatch_queue_t __PMKDefaultDispatchQueue;

dispatch_queue_t PMKDefaultDispatchQueue() {
    dispatch_once(&__PMKDefaultDispatchQueueToken, ^{
        if (__PMKDefaultDispatchQueue == nil) {
            __PMKDefaultDispatchQueue = dispatch_get_main_queue();
        }
    });
    return __PMKDefaultDispatchQueue;
}

void PMKSetDefaultDispatchQueue(dispatch_queue_t newDefaultQueue) {
    dispatch_once(&__PMKDefaultDispatchQueueToken, ^{
        __PMKDefaultDispatchQueue = newDefaultQueue;
    });
}


static dispatch_once_t __PMKErrorUnhandlerToken;
static void (^__PMKErrorUnhandler)(NSError *);

void PMKUnhandledErrorHandler(NSError *error) {
    dispatch_once(&__PMKErrorUnhandlerToken, ^{
        if (__PMKErrorUnhandler == nil) {
            __PMKErrorUnhandler = ^(NSError *error){
                if (!error.isCancelled) {
                    NSLog(@"PromiseKit: unhandled error: %@", error);
                }
            };
        }
    });
    return __PMKErrorUnhandler(error);
}

void PMKSetUnhandledErrorHandler(void(^newHandler)(NSError *)) {
    dispatch_once(&__PMKErrorUnhandlerToken, ^{
        __PMKErrorUnhandler = newHandler;
    });
}


static dispatch_once_t __PMKUnhandledExceptionHandlerToken;
static NSError *(^__PMKUnhandledExceptionHandler)(id);

NSError *PMKProcessUnhandledException(id thrown) {

    dispatch_once(&__PMKUnhandledExceptionHandlerToken, ^{
        __PMKUnhandledExceptionHandler = ^id(id reason){
            if ([reason isKindOfClass:[NSError class]])
                return reason;
            if ([reason isKindOfClass:[NSString class]])
                return [NSError errorWithDomain:PMKErrorDomain code:PMKUnexpectedError userInfo:@{NSLocalizedDescriptionKey: reason}];
            return nil;
        };
    });

    id err = __PMKUnhandledExceptionHandler(thrown);
    if (!err) {
        NSLog(@"PromiseKit no longer catches *all* exceptions. However you can change this behavior by setting a new PMKProcessUnhandledException handler.");
        @throw thrown;
    }
    return err;
}

void PMKSetUnhandledExceptionHandler(NSError *(^newHandler)(id)) {
    dispatch_once(&__PMKUnhandledExceptionHandlerToken, ^{
        __PMKUnhandledExceptionHandler = newHandler;
    });
}
