package io.swagger.petstore.test;

import junit.framework.TestFailure;
import rx.Subscriber;

/**
 * Skeleton subscriber for tests that will fail when onError() is called unexpectedly.
 */
public abstract class SkeletonSubscriber<T> extends Subscriber<T> {

    public static <T> SkeletonSubscriber<T> failTestOnError() {
        return new SkeletonSubscriber<T>() {
        };
    }

    @Override
    public void onCompleted() {
        // space for rent
    }

    @Override
    public void onNext(T t) {
        // space for rent
    }

    @Override
    public void onError(Throwable e) {
        throw new RuntimeException("Subscriber onError() called with unhandled exception!", e);
    }
}
