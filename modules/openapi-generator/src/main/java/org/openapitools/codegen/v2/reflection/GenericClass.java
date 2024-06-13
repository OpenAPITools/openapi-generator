package org.openapitools.codegen.v2.reflection;

import com.google.common.reflect.TypeToken;

@SuppressWarnings("UnstableApiUsage")
public abstract class GenericClass<T> {
    private final TypeToken<T> typeToken;

    public GenericClass() {
        this.typeToken = new TypeToken<T>(getClass()) {};
    }

    @SuppressWarnings("unchecked")
    public final Class<T> getGenericClass() {
        return (Class<T>) typeToken.getRawType();
    }

//    public static <T> GenericClass<T> of() {
//        return new GenericClass<T>() { };
//    }
}