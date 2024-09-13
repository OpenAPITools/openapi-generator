package org.openapitools.codegen.v2;

import org.openapitools.codegen.v2.reflection.GenericClass;

import javax.annotation.Nonnull;
import java.util.Locale;
import java.util.Objects;

public final class CodegenTag implements Comparable<CodegenTag> {

    private final String name;
    private final Class<?> valueClass;
    private final GenericClass<?> valueGenericClass;

    private CodegenTag(String name, GenericClass<?> valueGenericClass) {
        this.name = name;
        this.valueClass = null;
        this.valueGenericClass = valueGenericClass;
    }

    private CodegenTag(String name, Class<?> valueClass) {
        this.name = name;
        this.valueClass = valueClass;
        this.valueGenericClass = null;
    }

    @Override
    public int compareTo(@Nonnull CodegenTag tag) {
        return name.compareTo(tag.name);
    }

    @Override
    public boolean equals(@Nonnull Object obj) {
        if (obj instanceof CodegenTag) {
            CodegenTag tag = (CodegenTag) obj;
            return 0 == compareTo(tag);
        }
        return false;
    }

    @Override
    public int hashCode() {
        return name.hashCode();
    }

    public String getName() {
        return name;
    }

    public Class<?> getValueClass() {
        return valueGenericClass != null ? valueGenericClass.getGenericClass() : valueClass;
    }

    public GenericClass<?> getValueGenericClass() {
        return valueGenericClass;
    }

    public <TValue> boolean acceptsValue(TValue value) {
        Objects.requireNonNull(value);
        return acceptsValueClass(value.getClass());
    }

    public <TValue> boolean acceptsValueGenericClass(GenericClass<TValue> valueGenericClass) {
        Objects.requireNonNull(valueGenericClass);
        return acceptsValueClass(valueGenericClass.getGenericClass());
    }

    public <TValue> boolean acceptsValueClass(Class<TValue> valueClass) {
        Objects.requireNonNull(valueClass);
        return getValueClass().isAssignableFrom(valueClass);
    }

    public <TValue> void ensureValueAccepted(TValue value) {
        Objects.requireNonNull(value);
        ensureValueClassAccepted(value.getClass());
    }

    public <TValue> void ensureValueAccepted(GenericClass<TValue> valueGenericClass) {
        Objects.requireNonNull(valueGenericClass);
        ensureValueClassAccepted(valueGenericClass.getGenericClass());
    }

    public <TValue> void ensureValueClassAccepted(Class<TValue> valueClass) {
        Objects.requireNonNull(valueClass);
        if (!acceptsValueClass(valueClass)) {
            String message = String.format(Locale.getDefault(),
                    "Unexpected tag value of class '%s', expected '%s'",
                    valueClass.getName(), getValueClass().getName());
            throw new IllegalArgumentException(message);
        }
    }

    public static CodegenTag of(String name) {
        Objects.requireNonNull(name);
        return of(name, Object.class);
    }

    public static <TValue> CodegenTag of(String name, Class<TValue> valueClass) {
        Objects.requireNonNull(name);
        Objects.requireNonNull(valueClass);
        return new CodegenTag(name, valueClass);
    }

    public static <TValue> CodegenTag of(String name, GenericClass<TValue> valueGenericClass) {
        Objects.requireNonNull(name);
        Objects.requireNonNull(valueGenericClass);
        return new CodegenTag(name, valueGenericClass);
    }
}
