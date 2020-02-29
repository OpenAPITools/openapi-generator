package org.openapitools.client.models;

import java.lang.System;

/**
 * A pet for sale in the pet store
 * @param name 
 * @param photoUrls 
 * @param id 
 * @param category 
 * @param tags 
 * @param status pet status in the store
 */
@kotlin.Metadata(mv = {1, 1, 16}, bv = {1, 0, 3}, k = 1, d1 = {"\u0000@\n\u0002\u0018\u0002\n\u0002\u0010\u0000\n\u0000\n\u0002\u0010\u000e\n\u0000\n\u0002\u0010\u0011\n\u0000\n\u0002\u0010\t\n\u0000\n\u0002\u0018\u0002\n\u0000\n\u0002\u0018\u0002\n\u0000\n\u0002\u0018\u0002\n\u0002\b\u0019\n\u0002\u0010\u000b\n\u0002\b\u0002\n\u0002\u0010\b\n\u0002\b\u0003\b\u0087\b\u0018\u00002\u00020\u0001:\u0001,BU\u0012\b\b\u0001\u0010\u0002\u001a\u00020\u0003\u0012\u000e\b\u0001\u0010\u0004\u001a\b\u0012\u0004\u0012\u00020\u00030\u0005\u0012\n\b\u0003\u0010\u0006\u001a\u0004\u0018\u00010\u0007\u0012\n\b\u0003\u0010\b\u001a\u0004\u0018\u00010\t\u0012\u0010\b\u0003\u0010\n\u001a\n\u0012\u0004\u0012\u00020\u000b\u0018\u00010\u0005\u0012\n\b\u0003\u0010\f\u001a\u0004\u0018\u00010\r\u00a2\u0006\u0002\u0010\u000eJ\t\u0010\u001e\u001a\u00020\u0003H\u00c6\u0003J\u0014\u0010\u001f\u001a\b\u0012\u0004\u0012\u00020\u00030\u0005H\u00c6\u0003\u00a2\u0006\u0002\u0010\u0017J\u0010\u0010 \u001a\u0004\u0018\u00010\u0007H\u00c6\u0003\u00a2\u0006\u0002\u0010\u0012J\u000b\u0010!\u001a\u0004\u0018\u00010\tH\u00c6\u0003J\u0016\u0010\"\u001a\n\u0012\u0004\u0012\u00020\u000b\u0018\u00010\u0005H\u00c6\u0003\u00a2\u0006\u0002\u0010\u001cJ\u000b\u0010#\u001a\u0004\u0018\u00010\rH\u00c6\u0003J^\u0010$\u001a\u00020\u00002\b\b\u0003\u0010\u0002\u001a\u00020\u00032\u000e\b\u0003\u0010\u0004\u001a\b\u0012\u0004\u0012\u00020\u00030\u00052\n\b\u0003\u0010\u0006\u001a\u0004\u0018\u00010\u00072\n\b\u0003\u0010\b\u001a\u0004\u0018\u00010\t2\u0010\b\u0003\u0010\n\u001a\n\u0012\u0004\u0012\u00020\u000b\u0018\u00010\u00052\n\b\u0003\u0010\f\u001a\u0004\u0018\u00010\rH\u00c6\u0001\u00a2\u0006\u0002\u0010%J\u0013\u0010&\u001a\u00020\'2\b\u0010(\u001a\u0004\u0018\u00010\u0001H\u00d6\u0003J\t\u0010)\u001a\u00020*H\u00d6\u0001J\t\u0010+\u001a\u00020\u0003H\u00d6\u0001R\u0013\u0010\b\u001a\u0004\u0018\u00010\t\u00a2\u0006\b\n\u0000\u001a\u0004\b\u000f\u0010\u0010R\u0015\u0010\u0006\u001a\u0004\u0018\u00010\u0007\u00a2\u0006\n\n\u0002\u0010\u0013\u001a\u0004\b\u0011\u0010\u0012R\u0011\u0010\u0002\u001a\u00020\u0003\u00a2\u0006\b\n\u0000\u001a\u0004\b\u0014\u0010\u0015R\u0019\u0010\u0004\u001a\b\u0012\u0004\u0012\u00020\u00030\u0005\u00a2\u0006\n\n\u0002\u0010\u0018\u001a\u0004\b\u0016\u0010\u0017R\u0013\u0010\f\u001a\u0004\u0018\u00010\r\u00a2\u0006\b\n\u0000\u001a\u0004\b\u0019\u0010\u001aR\u001b\u0010\n\u001a\n\u0012\u0004\u0012\u00020\u000b\u0018\u00010\u0005\u00a2\u0006\n\n\u0002\u0010\u001d\u001a\u0004\b\u001b\u0010\u001c\u00a8\u0006-"}, d2 = {"Lorg/openapitools/client/models/Pet;", "", "name", "", "photoUrls", "", "id", "", "category", "Lorg/openapitools/client/models/Category;", "tags", "Lorg/openapitools/client/models/Tag;", "status", "Lorg/openapitools/client/models/Pet$Status;", "(Ljava/lang/String;[Ljava/lang/String;Ljava/lang/Long;Lorg/openapitools/client/models/Category;[Lorg/openapitools/client/models/Tag;Lorg/openapitools/client/models/Pet$Status;)V", "getCategory", "()Lorg/openapitools/client/models/Category;", "getId", "()Ljava/lang/Long;", "Ljava/lang/Long;", "getName", "()Ljava/lang/String;", "getPhotoUrls", "()[Ljava/lang/String;", "[Ljava/lang/String;", "getStatus", "()Lorg/openapitools/client/models/Pet$Status;", "getTags", "()[Lorg/openapitools/client/models/Tag;", "[Lorg/openapitools/client/models/Tag;", "component1", "component2", "component3", "component4", "component5", "component6", "copy", "(Ljava/lang/String;[Ljava/lang/String;Ljava/lang/Long;Lorg/openapitools/client/models/Category;[Lorg/openapitools/client/models/Tag;Lorg/openapitools/client/models/Pet$Status;)Lorg/openapitools/client/models/Pet;", "equals", "", "other", "hashCode", "", "toString", "Status", "kotlin-petstore-moshi-codegen"})
@com.squareup.moshi.JsonClass(generateAdapter = true)
public final class Pet {
    @org.jetbrains.annotations.NotNull()
    private final java.lang.String name = null;
    @org.jetbrains.annotations.NotNull()
    private final java.lang.String[] photoUrls = null;
    @org.jetbrains.annotations.Nullable()
    private final java.lang.Long id = null;
    @org.jetbrains.annotations.Nullable()
    private final org.openapitools.client.models.Category category = null;
    @org.jetbrains.annotations.Nullable()
    private final org.openapitools.client.models.Tag[] tags = null;
    @org.jetbrains.annotations.Nullable()
    private final org.openapitools.client.models.Pet.Status status = null;
    
    @org.jetbrains.annotations.NotNull()
    public final java.lang.String getName() {
        return null;
    }
    
    @org.jetbrains.annotations.NotNull()
    public final java.lang.String[] getPhotoUrls() {
        return null;
    }
    
    @org.jetbrains.annotations.Nullable()
    public final java.lang.Long getId() {
        return null;
    }
    
    @org.jetbrains.annotations.Nullable()
    public final org.openapitools.client.models.Category getCategory() {
        return null;
    }
    
    @org.jetbrains.annotations.Nullable()
    public final org.openapitools.client.models.Tag[] getTags() {
        return null;
    }
    
    @org.jetbrains.annotations.Nullable()
    public final org.openapitools.client.models.Pet.Status getStatus() {
        return null;
    }
    
    public Pet(@org.jetbrains.annotations.NotNull()
    @com.squareup.moshi.Json(name = "name")
    java.lang.String name, @org.jetbrains.annotations.NotNull()
    @com.squareup.moshi.Json(name = "photoUrls")
    java.lang.String[] photoUrls, @org.jetbrains.annotations.Nullable()
    @com.squareup.moshi.Json(name = "id")
    java.lang.Long id, @org.jetbrains.annotations.Nullable()
    @com.squareup.moshi.Json(name = "category")
    org.openapitools.client.models.Category category, @org.jetbrains.annotations.Nullable()
    @com.squareup.moshi.Json(name = "tags")
    org.openapitools.client.models.Tag[] tags, @org.jetbrains.annotations.Nullable()
    @com.squareup.moshi.Json(name = "status")
    org.openapitools.client.models.Pet.Status status) {
        super();
    }
    
    @org.jetbrains.annotations.NotNull()
    public final java.lang.String component1() {
        return null;
    }
    
    @org.jetbrains.annotations.NotNull()
    public final java.lang.String[] component2() {
        return null;
    }
    
    @org.jetbrains.annotations.Nullable()
    public final java.lang.Long component3() {
        return null;
    }
    
    @org.jetbrains.annotations.Nullable()
    public final org.openapitools.client.models.Category component4() {
        return null;
    }
    
    @org.jetbrains.annotations.Nullable()
    public final org.openapitools.client.models.Tag[] component5() {
        return null;
    }
    
    @org.jetbrains.annotations.Nullable()
    public final org.openapitools.client.models.Pet.Status component6() {
        return null;
    }
    
    /**
     * A pet for sale in the pet store
     * @param name 
     * @param photoUrls 
     * @param id 
     * @param category 
     * @param tags 
     * @param status pet status in the store
     */
    @org.jetbrains.annotations.NotNull()
    public final org.openapitools.client.models.Pet copy(@org.jetbrains.annotations.NotNull()
    @com.squareup.moshi.Json(name = "name")
    java.lang.String name, @org.jetbrains.annotations.NotNull()
    @com.squareup.moshi.Json(name = "photoUrls")
    java.lang.String[] photoUrls, @org.jetbrains.annotations.Nullable()
    @com.squareup.moshi.Json(name = "id")
    java.lang.Long id, @org.jetbrains.annotations.Nullable()
    @com.squareup.moshi.Json(name = "category")
    org.openapitools.client.models.Category category, @org.jetbrains.annotations.Nullable()
    @com.squareup.moshi.Json(name = "tags")
    org.openapitools.client.models.Tag[] tags, @org.jetbrains.annotations.Nullable()
    @com.squareup.moshi.Json(name = "status")
    org.openapitools.client.models.Pet.Status status) {
        return null;
    }
    
    /**
     * A pet for sale in the pet store
     * @param name 
     * @param photoUrls 
     * @param id 
     * @param category 
     * @param tags 
     * @param status pet status in the store
     */
    @org.jetbrains.annotations.NotNull()
    @java.lang.Override()
    public java.lang.String toString() {
        return null;
    }
    
    /**
     * A pet for sale in the pet store
     * @param name 
     * @param photoUrls 
     * @param id 
     * @param category 
     * @param tags 
     * @param status pet status in the store
     */
    @java.lang.Override()
    public int hashCode() {
        return 0;
    }
    
    /**
     * A pet for sale in the pet store
     * @param name 
     * @param photoUrls 
     * @param id 
     * @param category 
     * @param tags 
     * @param status pet status in the store
     */
    @java.lang.Override()
    public boolean equals(@org.jetbrains.annotations.Nullable()
    java.lang.Object p0) {
        return false;
    }
    
    /**
     * pet status in the store
     * Values: available,pending,sold
     */
    @kotlin.Metadata(mv = {1, 1, 16}, bv = {1, 0, 3}, k = 1, d1 = {"\u0000\u0012\n\u0002\u0018\u0002\n\u0002\u0010\u0010\n\u0000\n\u0002\u0010\u000e\n\u0002\b\u0007\b\u0086\u0001\u0018\u00002\b\u0012\u0004\u0012\u00020\u00000\u0001B\u000f\b\u0002\u0012\u0006\u0010\u0002\u001a\u00020\u0003\u00a2\u0006\u0002\u0010\u0004R\u0011\u0010\u0002\u001a\u00020\u0003\u00a2\u0006\b\n\u0000\u001a\u0004\b\u0005\u0010\u0006j\u0002\b\u0007j\u0002\b\bj\u0002\b\t\u00a8\u0006\n"}, d2 = {"Lorg/openapitools/client/models/Pet$Status;", "", "value", "", "(Ljava/lang/String;ILjava/lang/String;)V", "getValue", "()Ljava/lang/String;", "available", "pending", "sold", "kotlin-petstore-moshi-codegen"})
    public static enum Status {
        @com.squareup.moshi.Json(name = "available")
        /*public static final*/ available /* = new available(null) */,
        @com.squareup.moshi.Json(name = "pending")
        /*public static final*/ pending /* = new pending(null) */,
        @com.squareup.moshi.Json(name = "sold")
        /*public static final*/ sold /* = new sold(null) */;
        @org.jetbrains.annotations.NotNull()
        private final java.lang.String value = null;
        
        @org.jetbrains.annotations.NotNull()
        public final java.lang.String getValue() {
            return null;
        }
        
        Status(java.lang.String value) {
        }
    }
}