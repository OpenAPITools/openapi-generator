package org.openapitools.client.infrastructure;

import java.lang.System;

@kotlin.Metadata(mv = {1, 1, 16}, bv = {1, 0, 3}, k = 1, d1 = {"\u0000\u0014\n\u0002\u0018\u0002\n\u0002\u0010\u0000\n\u0002\b\u0002\n\u0002\u0018\u0002\n\u0002\b\u0005\b\u00c6\u0002\u0018\u00002\u00020\u0001:\u0001\bB\u0007\b\u0002\u00a2\u0006\u0002\u0010\u0002J+\u0010\u0003\u001a\u0010\u0012\u0006\u0012\u0004\u0018\u00010\u0001\u0012\u0004\u0012\u0002H\u00050\u0004\"\u0004\b\u0000\u0010\u00052\n\b\u0002\u0010\u0006\u001a\u0004\u0018\u0001H\u0005\u00a2\u0006\u0002\u0010\u0007\u00a8\u0006\t"}, d2 = {"Lorg/openapitools/client/infrastructure/ApplicationDelegates;", "", "()V", "setOnce", "Lkotlin/properties/ReadWriteProperty;", "T", "defaultValue", "(Ljava/lang/Object;)Lkotlin/properties/ReadWriteProperty;", "SetOnce", "kotlin-petstore-moshi-codegen"})
public final class ApplicationDelegates {
    public static final org.openapitools.client.infrastructure.ApplicationDelegates INSTANCE = null;
    
    /**
     * Provides a property delegate, allowing the property to be set once and only once.
     *
     * If unset (no default value), a get on the property will throw [IllegalStateException].
     */
    @org.jetbrains.annotations.NotNull()
    public final <T extends java.lang.Object>kotlin.properties.ReadWriteProperty<java.lang.Object, T> setOnce(@org.jetbrains.annotations.Nullable()
    T defaultValue) {
        return null;
    }
    
    private ApplicationDelegates() {
        super();
    }
    
    @kotlin.Metadata(mv = {1, 1, 16}, bv = {1, 0, 3}, k = 1, d1 = {"\u0000*\n\u0002\u0018\u0002\n\u0000\n\u0002\u0018\u0002\n\u0002\u0010\u0000\n\u0002\b\u0003\n\u0002\u0010\u000b\n\u0002\b\u0005\n\u0002\u0018\u0002\n\u0002\b\u0002\n\u0002\u0010\u0002\n\u0002\b\u0002\b\u0002\u0018\u0000*\u0004\b\u0000\u0010\u00012\u0010\u0012\u0006\u0012\u0004\u0018\u00010\u0003\u0012\u0004\u0012\u0002H\u00010\u0002B\u0011\u0012\n\b\u0002\u0010\u0004\u001a\u0004\u0018\u00018\u0000\u00a2\u0006\u0002\u0010\u0005J$\u0010\n\u001a\u00028\u00002\b\u0010\u000b\u001a\u0004\u0018\u00010\u00032\n\u0010\f\u001a\u0006\u0012\u0002\b\u00030\rH\u0096\u0002\u00a2\u0006\u0002\u0010\u000eJ,\u0010\u000f\u001a\u00020\u00102\b\u0010\u000b\u001a\u0004\u0018\u00010\u00032\n\u0010\f\u001a\u0006\u0012\u0002\b\u00030\r2\u0006\u0010\b\u001a\u00028\u0000H\u0096\u0002\u00a2\u0006\u0002\u0010\u0011R\u000e\u0010\u0006\u001a\u00020\u0007X\u0082\u000e\u00a2\u0006\u0002\n\u0000R\u0012\u0010\b\u001a\u0004\u0018\u00018\u0000X\u0082\u000e\u00a2\u0006\u0004\n\u0002\u0010\t\u00a8\u0006\u0012"}, d2 = {"Lorg/openapitools/client/infrastructure/ApplicationDelegates$SetOnce;", "T", "Lkotlin/properties/ReadWriteProperty;", "", "defaultValue", "(Ljava/lang/Object;)V", "isSet", "", "value", "Ljava/lang/Object;", "getValue", "thisRef", "property", "Lkotlin/reflect/KProperty;", "(Ljava/lang/Object;Lkotlin/reflect/KProperty;)Ljava/lang/Object;", "setValue", "", "(Ljava/lang/Object;Lkotlin/reflect/KProperty;Ljava/lang/Object;)V", "kotlin-petstore-moshi-codegen"})
    static final class SetOnce<T extends java.lang.Object> implements kotlin.properties.ReadWriteProperty<java.lang.Object, T> {
        private boolean isSet;
        private T value;
        
        @java.lang.Override()
        public T getValue(@org.jetbrains.annotations.Nullable()
        java.lang.Object thisRef, @org.jetbrains.annotations.NotNull()
        kotlin.reflect.KProperty<?> property) {
            return null;
        }
        
        @java.lang.Override()
        public void setValue(@org.jetbrains.annotations.Nullable()
        java.lang.Object thisRef, @org.jetbrains.annotations.NotNull()
        kotlin.reflect.KProperty<?> property, T value) {
        }
        
        public SetOnce(@org.jetbrains.annotations.Nullable()
        T defaultValue) {
            super();
        }
        
        public SetOnce() {
            super();
        }
    }
}