package io.swagger.client;

import java.util.concurrent.CompletionStage;
import retrofit2.*;

import java.lang.annotation.Annotation;
import java.lang.reflect.ParameterizedType;
import java.lang.reflect.Type;
import java.lang.reflect.WildcardType;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.CompletionStage;
import java.util.function.Function;

/**
 * Creates {@link CallAdapter} instances that convert {@link Call} into {@link java.util.concurrent.CompletionStage}
 */
public class Play25CallAdapterFactory extends CallAdapter.Factory {

    private Function<RuntimeException, RuntimeException> exceptionConverter = Function.identity();

    public Play25CallAdapterFactory() {
    }

    public Play25CallAdapterFactory(
            Function<RuntimeException, RuntimeException> exceptionConverter) {
        this.exceptionConverter = exceptionConverter;
    }

    @Override
    public CallAdapter<?, ?> get(Type returnType, Annotation[] annotations, Retrofit retrofit) {
        if (!(returnType instanceof ParameterizedType)) {
            return null;
        }

        ParameterizedType type = (ParameterizedType) returnType;
        if (type.getRawType() != CompletionStage.class) {
            return null;
        }

        return createAdapter((ParameterizedType) returnType);
    }

    private CallAdapter<?, CompletionStage<?>> createAdapter(ParameterizedType returnType) {
        // Get CompletionStage type argument
        Type[] types = returnType.getActualTypeArguments();
        if (types.length != 1) {
            throw new IllegalStateException("Must be exactly one type parameter");
        }

        Type resultType = types[0];
        Class<?> rawTypeParam = getRawType(resultType);

        boolean includeResponse = false;
        if (rawTypeParam == Response.class) {
            if (!(resultType instanceof ParameterizedType)) {
                throw new IllegalStateException("Response must be parameterized"
                        + " as Response<T>");
            }
            resultType = ((ParameterizedType) resultType).getActualTypeArguments()[0];
            includeResponse = true;
        }

        return new ValueAdapter(resultType, includeResponse, exceptionConverter);
    }

    /**
     * Adpater that coverts values returned by API interface into CompletionStage
     */
    private static final class ValueAdapter<R> implements CallAdapter<R, CompletionStage<R>> {

        private final Type responseType;
        private final boolean includeResponse;
        private Function<RuntimeException, RuntimeException> exceptionConverter;

        ValueAdapter(Type responseType, boolean includeResponse,
                     Function<RuntimeException, RuntimeException> exceptionConverter) {
            this.responseType = responseType;
            this.includeResponse = includeResponse;
            this.exceptionConverter = exceptionConverter;
        }

        @Override
        public Type responseType() {
            return responseType;
        }

        @Override
        public CompletionStage<R> adapt(final Call<R> call) {
            final CompletableFuture<R> promise = new CompletableFuture();

            call.enqueue(new Callback<R>() {

                @Override
                public void onResponse(Call<R> call, Response<R> response) {
                    if (response.isSuccessful()) {
                        if (includeResponse) {
                            promise.complete((R) response);
                        } else {
                            promise.complete(response.body());
                        }
                    } else {
                        promise.completeExceptionally(exceptionConverter.apply(new HttpException(response)));
                    }
                }

                @Override
                public void onFailure(Call<R> call, Throwable t) {
                    promise.completeExceptionally(t);
                }

            });

            return promise;
        }
    }
}

