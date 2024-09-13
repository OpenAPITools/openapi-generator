package org.openapitools.codegen.v2;

import org.openapitools.codegen.v2.reflection.GenericClass;

import java.util.Map;
import java.util.Objects;
import java.util.function.Supplier;

public interface CodegenTaggable {
    Map<CodegenTag, Object> getTags();

    default void setTag(CodegenTag tag, Object value) {
        Objects.requireNonNull(tag);
        Objects.requireNonNull(value);
        tag.ensureValueClassAccepted(value.getClass());
        getTags().put(tag, value);
    }

    default void unsetTag(CodegenTag tag) {
        Objects.requireNonNull(tag);
        getTags().remove(tag);
    }

    default Object getTag(CodegenTag tag) {
        Objects.requireNonNull(tag);
        return getTags().getOrDefault(tag, null);
    }

    default <TValue> TValue getTag(CodegenTag tag, GenericClass<TValue> valueGenericClass) {
        Objects.requireNonNull(tag);
        Objects.requireNonNull(valueGenericClass);
        return getTag(tag, valueGenericClass.getGenericClass());
    }

    default <TValue> TValue getTag(CodegenTag tag, Class<TValue> valueClass) {
        Objects.requireNonNull(tag);
        Objects.requireNonNull(valueClass);
        Object value = getTag(tag);
        return value != null && valueClass.isAssignableFrom(value.getClass()) ? valueClass.cast(value) : null;
    }

    default Object getTagOrDefault(CodegenTag tag, Supplier supplier) {
        Objects.requireNonNull(tag);
        Objects.requireNonNull(supplier);
        Object value = getTag(tag);
        if (value == null) {
            value = supplier.get();
            setTag(tag, value);
        }
        return value;
    }

    default <TValue> TValue getTagOrDefault(CodegenTag tag, GenericClass<TValue> valueGenericClass, Supplier<TValue> supplier) {
        Objects.requireNonNull(tag);
        Objects.requireNonNull(valueGenericClass);
        Objects.requireNonNull(supplier);
        return getTagOrDefault(tag, valueGenericClass.getGenericClass(), supplier);
    }

    default <TValue> TValue getTagOrDefault(CodegenTag tag, Class<TValue> valueClass, Supplier<TValue> supplier) {
        Objects.requireNonNull(tag);
        Objects.requireNonNull(valueClass);
        Objects.requireNonNull(supplier);
        Object value = getTagOrDefault(tag, supplier);
        return value != null && valueClass.isAssignableFrom(value.getClass()) ? valueClass.cast(value) : null;
    }

    default boolean hasTag(CodegenTag tag) {
        return getTags().containsKey(tag);
    }
}
