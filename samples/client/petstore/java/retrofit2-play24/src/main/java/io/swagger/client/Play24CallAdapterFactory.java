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

    private Type getTypeParam(ParameterizedType type) {
        Type[] types = type.getActualTypeArguments();
        if (types.length != 1) {
            throw new IllegalStateException("Must be exactly one type parameter");
        }

        Type paramType = types[0];
        if (paramType instanceof WildcardType) {
            return ((WildcardType) paramType).getUpperBounds()[0];
        }

        return paramType;
    }

    private CallAdapter<?, F.Promise<?>> createAdapter(ParameterizedType returnType) {
        Type parameterType = getTypeParam(returnType);
        return new ValueAdapter(parameterType);
    }

    /**
     * Adpater that coverts values returned by API interface into Play promises
     */
    static final class ValueAdapter<R> implements CallAdapter<R, F.Promise<R>> {

        private final Type responseType;

        ValueAdapter(Type responseType) {
            this.responseType = responseType;
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
                        promise.success(response.body());
                    } else {
                        promise.failure(new Exception(response.errorBody().toString()));
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
