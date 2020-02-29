package org.openapitools.client.models;

import java.lang.System;

/**
 * An order for a pets from the pet store
 * @param id 
 * @param petId 
 * @param quantity 
 * @param shipDate 
 * @param status Order Status
 * @param complete
 */
@kotlin.Metadata(mv = {1, 1, 16}, bv = {1, 0, 3}, k = 1, d1 = {"\u00004\n\u0002\u0018\u0002\n\u0002\u0010\u0000\n\u0000\n\u0002\u0010\t\n\u0002\b\u0002\n\u0002\u0010\b\n\u0000\n\u0002\u0018\u0002\n\u0000\n\u0002\u0018\u0002\n\u0000\n\u0002\u0010\u000b\n\u0002\b\u001b\n\u0002\u0010\u000e\n\u0002\b\u0002\b\u0087\b\u0018\u00002\u00020\u0001:\u0001)BM\u0012\n\b\u0003\u0010\u0002\u001a\u0004\u0018\u00010\u0003\u0012\n\b\u0003\u0010\u0004\u001a\u0004\u0018\u00010\u0003\u0012\n\b\u0003\u0010\u0005\u001a\u0004\u0018\u00010\u0006\u0012\n\b\u0003\u0010\u0007\u001a\u0004\u0018\u00010\b\u0012\n\b\u0003\u0010\t\u001a\u0004\u0018\u00010\n\u0012\n\b\u0003\u0010\u000b\u001a\u0004\u0018\u00010\f\u00a2\u0006\u0002\u0010\rJ\u0010\u0010\u001c\u001a\u0004\u0018\u00010\u0003H\u00c6\u0003\u00a2\u0006\u0002\u0010\u0012J\u0010\u0010\u001d\u001a\u0004\u0018\u00010\u0003H\u00c6\u0003\u00a2\u0006\u0002\u0010\u0012J\u0010\u0010\u001e\u001a\u0004\u0018\u00010\u0006H\u00c6\u0003\u00a2\u0006\u0002\u0010\u0016J\u000b\u0010\u001f\u001a\u0004\u0018\u00010\bH\u00c6\u0003J\u000b\u0010 \u001a\u0004\u0018\u00010\nH\u00c6\u0003J\u0010\u0010!\u001a\u0004\u0018\u00010\fH\u00c6\u0003\u00a2\u0006\u0002\u0010\u000fJV\u0010\"\u001a\u00020\u00002\n\b\u0003\u0010\u0002\u001a\u0004\u0018\u00010\u00032\n\b\u0003\u0010\u0004\u001a\u0004\u0018\u00010\u00032\n\b\u0003\u0010\u0005\u001a\u0004\u0018\u00010\u00062\n\b\u0003\u0010\u0007\u001a\u0004\u0018\u00010\b2\n\b\u0003\u0010\t\u001a\u0004\u0018\u00010\n2\n\b\u0003\u0010\u000b\u001a\u0004\u0018\u00010\fH\u00c6\u0001\u00a2\u0006\u0002\u0010#J\u0013\u0010$\u001a\u00020\f2\b\u0010%\u001a\u0004\u0018\u00010\u0001H\u00d6\u0003J\t\u0010&\u001a\u00020\u0006H\u00d6\u0001J\t\u0010\'\u001a\u00020(H\u00d6\u0001R\u0015\u0010\u000b\u001a\u0004\u0018\u00010\f\u00a2\u0006\n\n\u0002\u0010\u0010\u001a\u0004\b\u000e\u0010\u000fR\u0015\u0010\u0002\u001a\u0004\u0018\u00010\u0003\u00a2\u0006\n\n\u0002\u0010\u0013\u001a\u0004\b\u0011\u0010\u0012R\u0015\u0010\u0004\u001a\u0004\u0018\u00010\u0003\u00a2\u0006\n\n\u0002\u0010\u0013\u001a\u0004\b\u0014\u0010\u0012R\u0015\u0010\u0005\u001a\u0004\u0018\u00010\u0006\u00a2\u0006\n\n\u0002\u0010\u0017\u001a\u0004\b\u0015\u0010\u0016R\u0013\u0010\u0007\u001a\u0004\u0018\u00010\b\u00a2\u0006\b\n\u0000\u001a\u0004\b\u0018\u0010\u0019R\u0013\u0010\t\u001a\u0004\u0018\u00010\n\u00a2\u0006\b\n\u0000\u001a\u0004\b\u001a\u0010\u001b\u00a8\u0006*"}, d2 = {"Lorg/openapitools/client/models/Order;", "", "id", "", "petId", "quantity", "", "shipDate", "Ljava/time/OffsetDateTime;", "status", "Lorg/openapitools/client/models/Order$Status;", "complete", "", "(Ljava/lang/Long;Ljava/lang/Long;Ljava/lang/Integer;Ljava/time/OffsetDateTime;Lorg/openapitools/client/models/Order$Status;Ljava/lang/Boolean;)V", "getComplete", "()Ljava/lang/Boolean;", "Ljava/lang/Boolean;", "getId", "()Ljava/lang/Long;", "Ljava/lang/Long;", "getPetId", "getQuantity", "()Ljava/lang/Integer;", "Ljava/lang/Integer;", "getShipDate", "()Ljava/time/OffsetDateTime;", "getStatus", "()Lorg/openapitools/client/models/Order$Status;", "component1", "component2", "component3", "component4", "component5", "component6", "copy", "(Ljava/lang/Long;Ljava/lang/Long;Ljava/lang/Integer;Ljava/time/OffsetDateTime;Lorg/openapitools/client/models/Order$Status;Ljava/lang/Boolean;)Lorg/openapitools/client/models/Order;", "equals", "other", "hashCode", "toString", "", "Status", "kotlin-petstore-moshi-codegen"})
@com.squareup.moshi.JsonClass(generateAdapter = true)
public final class Order {
    @org.jetbrains.annotations.Nullable()
    private final java.lang.Long id = null;
    @org.jetbrains.annotations.Nullable()
    private final java.lang.Long petId = null;
    @org.jetbrains.annotations.Nullable()
    private final java.lang.Integer quantity = null;
    @org.jetbrains.annotations.Nullable()
    private final java.time.OffsetDateTime shipDate = null;
    @org.jetbrains.annotations.Nullable()
    private final org.openapitools.client.models.Order.Status status = null;
    @org.jetbrains.annotations.Nullable()
    private final java.lang.Boolean complete = null;
    
    @org.jetbrains.annotations.Nullable()
    public final java.lang.Long getId() {
        return null;
    }
    
    @org.jetbrains.annotations.Nullable()
    public final java.lang.Long getPetId() {
        return null;
    }
    
    @org.jetbrains.annotations.Nullable()
    public final java.lang.Integer getQuantity() {
        return null;
    }
    
    @org.jetbrains.annotations.Nullable()
    public final java.time.OffsetDateTime getShipDate() {
        return null;
    }
    
    @org.jetbrains.annotations.Nullable()
    public final org.openapitools.client.models.Order.Status getStatus() {
        return null;
    }
    
    @org.jetbrains.annotations.Nullable()
    public final java.lang.Boolean getComplete() {
        return null;
    }
    
    public Order(@org.jetbrains.annotations.Nullable()
    @com.squareup.moshi.Json(name = "id")
    java.lang.Long id, @org.jetbrains.annotations.Nullable()
    @com.squareup.moshi.Json(name = "petId")
    java.lang.Long petId, @org.jetbrains.annotations.Nullable()
    @com.squareup.moshi.Json(name = "quantity")
    java.lang.Integer quantity, @org.jetbrains.annotations.Nullable()
    @com.squareup.moshi.Json(name = "shipDate")
    java.time.OffsetDateTime shipDate, @org.jetbrains.annotations.Nullable()
    @com.squareup.moshi.Json(name = "status")
    org.openapitools.client.models.Order.Status status, @org.jetbrains.annotations.Nullable()
    @com.squareup.moshi.Json(name = "complete")
    java.lang.Boolean complete) {
        super();
    }
    
    public Order() {
        super();
    }
    
    @org.jetbrains.annotations.Nullable()
    public final java.lang.Long component1() {
        return null;
    }
    
    @org.jetbrains.annotations.Nullable()
    public final java.lang.Long component2() {
        return null;
    }
    
    @org.jetbrains.annotations.Nullable()
    public final java.lang.Integer component3() {
        return null;
    }
    
    @org.jetbrains.annotations.Nullable()
    public final java.time.OffsetDateTime component4() {
        return null;
    }
    
    @org.jetbrains.annotations.Nullable()
    public final org.openapitools.client.models.Order.Status component5() {
        return null;
    }
    
    @org.jetbrains.annotations.Nullable()
    public final java.lang.Boolean component6() {
        return null;
    }
    
    /**
     * An order for a pets from the pet store
     * @param id 
     * @param petId 
     * @param quantity 
     * @param shipDate 
     * @param status Order Status
     * @param complete
     */
    @org.jetbrains.annotations.NotNull()
    public final org.openapitools.client.models.Order copy(@org.jetbrains.annotations.Nullable()
    @com.squareup.moshi.Json(name = "id")
    java.lang.Long id, @org.jetbrains.annotations.Nullable()
    @com.squareup.moshi.Json(name = "petId")
    java.lang.Long petId, @org.jetbrains.annotations.Nullable()
    @com.squareup.moshi.Json(name = "quantity")
    java.lang.Integer quantity, @org.jetbrains.annotations.Nullable()
    @com.squareup.moshi.Json(name = "shipDate")
    java.time.OffsetDateTime shipDate, @org.jetbrains.annotations.Nullable()
    @com.squareup.moshi.Json(name = "status")
    org.openapitools.client.models.Order.Status status, @org.jetbrains.annotations.Nullable()
    @com.squareup.moshi.Json(name = "complete")
    java.lang.Boolean complete) {
        return null;
    }
    
    /**
     * An order for a pets from the pet store
     * @param id 
     * @param petId 
     * @param quantity 
     * @param shipDate 
     * @param status Order Status
     * @param complete
     */
    @org.jetbrains.annotations.NotNull()
    @java.lang.Override()
    public java.lang.String toString() {
        return null;
    }
    
    /**
     * An order for a pets from the pet store
     * @param id 
     * @param petId 
     * @param quantity 
     * @param shipDate 
     * @param status Order Status
     * @param complete
     */
    @java.lang.Override()
    public int hashCode() {
        return 0;
    }
    
    /**
     * An order for a pets from the pet store
     * @param id 
     * @param petId 
     * @param quantity 
     * @param shipDate 
     * @param status Order Status
     * @param complete
     */
    @java.lang.Override()
    public boolean equals(@org.jetbrains.annotations.Nullable()
    java.lang.Object p0) {
        return false;
    }
    
    /**
     * Order Status
     * Values: placed,approved,delivered
     */
    @kotlin.Metadata(mv = {1, 1, 16}, bv = {1, 0, 3}, k = 1, d1 = {"\u0000\u0012\n\u0002\u0018\u0002\n\u0002\u0010\u0010\n\u0000\n\u0002\u0010\u000e\n\u0002\b\u0007\b\u0086\u0001\u0018\u00002\b\u0012\u0004\u0012\u00020\u00000\u0001B\u000f\b\u0002\u0012\u0006\u0010\u0002\u001a\u00020\u0003\u00a2\u0006\u0002\u0010\u0004R\u0011\u0010\u0002\u001a\u00020\u0003\u00a2\u0006\b\n\u0000\u001a\u0004\b\u0005\u0010\u0006j\u0002\b\u0007j\u0002\b\bj\u0002\b\t\u00a8\u0006\n"}, d2 = {"Lorg/openapitools/client/models/Order$Status;", "", "value", "", "(Ljava/lang/String;ILjava/lang/String;)V", "getValue", "()Ljava/lang/String;", "placed", "approved", "delivered", "kotlin-petstore-moshi-codegen"})
    public static enum Status {
        @com.squareup.moshi.Json(name = "placed")
        /*public static final*/ placed /* = new placed(null) */,
        @com.squareup.moshi.Json(name = "approved")
        /*public static final*/ approved /* = new approved(null) */,
        @com.squareup.moshi.Json(name = "delivered")
        /*public static final*/ delivered /* = new delivered(null) */;
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