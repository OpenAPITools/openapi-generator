#import "AnyPromise.h"
@import Dispatch;

AnyPromise *dispatch_promise_on(dispatch_queue_t queue, id block) {
    return [AnyPromise promiseWithValue:nil].thenOn(queue, block);
}

AnyPromise *dispatch_promise(id block) {
    return dispatch_promise_on(dispatch_get_global_queue(0, 0), block);
}
