#import "AnyPromise.h"
#import "AnyPromise+Private.h"
@import CoreFoundation.CFRunLoop;

/**
 Suspends the active thread waiting on the provided promise.

 @return The value of the provided promise once resolved. 
 */
id PMKHang(AnyPromise *promise) {
    if (promise.pending) {
        static CFRunLoopSourceContext context;

        CFRunLoopRef runLoop = CFRunLoopGetCurrent();
        CFRunLoopSourceRef runLoopSource = CFRunLoopSourceCreate(NULL, 0, &context);
        CFRunLoopAddSource(runLoop, runLoopSource, kCFRunLoopDefaultMode);

        promise.always(^{
            CFRunLoopStop(runLoop);
        });
        while (promise.pending) {
            CFRunLoopRun();
        }
        CFRunLoopRemoveSource(runLoop, runLoopSource, kCFRunLoopDefaultMode);
        CFRelease(runLoopSource);
    }

    return promise.value;
}
