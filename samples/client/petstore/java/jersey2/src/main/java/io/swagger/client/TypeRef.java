package io.swagger.client;

import java.lang.reflect.ParameterizedType;
import java.lang.reflect.Type;

@javax.annotation.Generated(value = "class io.swagger.codegen.languages.JavaClientCodegen", date = "2015-10-20T11:29:47.599-07:00")
public class TypeRef<T> {
    private final Type type;

    public TypeRef() {
        this.type = getGenericType(getClass());
    }

    private static Type getGenericType(Class<?> klass) {
        Type superclass = klass.getGenericSuperclass();
        if (superclass instanceof Class) {
            throw new RuntimeException("No type parameter provided");
        }
        ParameterizedType parameterized = (ParameterizedType) superclass;
        return parameterized.getActualTypeArguments()[0];
    }

    public Type getType() {
        return type;
    }
}
