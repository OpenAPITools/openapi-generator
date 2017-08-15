package io.swagger.client;

import play.libs.F;
import retrofit2.*;

import java.lang.annotation.Annotation;
import java.lang.reflect.ParameterizedType;
import java.lang.reflect.Type;
import java.lang.reflect.WildcardType;

/**
 * Creates {@link CallAdapter} instances that convert {@link Call} into {@link play.libs.F.Promise}
 */
public class Play24CallAdapterFactory extends CallAdapter.Factory {

    @Override
    public CallAdapter<?, ?> get(Type returnType, Annotation[] annotations, Retrofit retrofit) {
        if (!(returnType instanceof ParameterizedType)) {
            return null;
        }

        ParameterizedType type = (ParameterizedType) returnType;
        if (type.getRawType() != F.Promise.class) {
            return null;
        }

        return createAdapter((ParameterizedType) returnType);
    }

    private CallAdapter<?, F.Promise<?>> createAdapter(ParameterizedType returnType) {
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

        return new ValueAdapter(resultType, includeResponse);
    }
    
    /**
     * Adpater that coverts values returned by API interface into CompletionStage
     */
    private static final class ValueAdapter<R> implements CallAdapter<R, F.Promise<R>> {

        private final Type responseType;
        private final boolean includeResponse;

        ValueAdapter(Type responseType, boolean includeResponse) {
            this.responseType = responseType;
            this.includeResponse = includeResponse;
        }

        @Override
        public Type responseType() {
            return responseType;
        }

        @Override
        public F.Promise<R> adapt(final Call<R> call) {
            final F.RedeemablePromise<R> promise = F.RedeemablePromise.empty();

            call.enqueue(new Callback<R>() {

                @Override
                public void onResponse(Call<R> call, Response<R> response) {
                    if (response.isSuccessful()) {
                        if (includeResponse) {
                            promise.success((R) response);
                        } else {
                            promise.success(response.body());
                        }
                    } else {
                        promise.failure(new HttpException(response));
                    }
                }

                @Override
                public void onFailure(Call<R> call, Throwable t) {
                    promise.failure(t);
                }

            });

            return promise;
        }
    }

}
