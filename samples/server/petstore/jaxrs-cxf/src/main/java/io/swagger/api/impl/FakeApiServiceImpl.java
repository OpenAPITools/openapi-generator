package io.swagger.api.impl;

import io.swagger.api.*;
import java.math.BigDecimal;
import io.swagger.model.Client;
import java.util.Date;
import org.joda.time.LocalDate;
import io.swagger.model.OuterComposite;

import java.io.InputStream;
import java.io.OutputStream;
import java.util.List;
import java.util.Map;
import javax.ws.rs.*;
import javax.ws.rs.core.Response;
import org.apache.cxf.jaxrs.model.wadl.Description;
import org.apache.cxf.jaxrs.model.wadl.DocTarget;

import org.apache.cxf.jaxrs.ext.multipart.*;

import io.swagger.annotations.Api;

/**
 * Swagger Petstore
 *
 * <p>This spec is mainly for testing Petstore server and contains fake endpoints, models. Please do not use this for any other purpose. Special characters: \" \\
 *
 */
public class FakeApiServiceImpl implements FakeApi {
    public Boolean fakeOuterBooleanSerialize(Boolean body) {
        // TODO: Implement...
        
        return null;
    }
    
    public OuterComposite fakeOuterCompositeSerialize(OuterComposite body) {
        // TODO: Implement...
        
        return null;
    }
    
    public BigDecimal fakeOuterNumberSerialize(BigDecimal body) {
        // TODO: Implement...
        
        return null;
    }
    
    public String fakeOuterStringSerialize(String body) {
        // TODO: Implement...
        
        return null;
    }
    
    /**
     * To test \&quot;client\&quot; model
     *
     * To test \&quot;client\&quot; model
     *
     */
    public Client testClientModel(Client body) {
        // TODO: Implement...
        
        return null;
    }
    
    /**
     * Fake endpoint for testing various parameters 假端點 偽のエンドポイント 가짜 엔드 포인트 
     *
     * Fake endpoint for testing various parameters 假端點 偽のエンドポイント 가짜 엔드 포인트 
     *
     */
    public void testEndpointParameters(BigDecimal number, Double _double, String patternWithoutDelimiter, byte[] _byte, Integer integer, Integer int32, Long int64, Float _float, String string, byte[] binary, LocalDate date, Date dateTime, String password, String paramCallback) {
        // TODO: Implement...
        
        
    }
    
    /**
     * To test enum parameters
     *
     * To test enum parameters
     *
     */
    public void testEnumParameters(List<String> enumFormStringArray, String enumFormString, List<String> enumHeaderStringArray, String enumHeaderString, List<String> enumQueryStringArray, String enumQueryString, Integer enumQueryInteger, Double enumQueryDouble) {
        // TODO: Implement...
        
        
    }
    
    /**
     * test inline additionalProperties
     *
     * 
     *
     */
    public void testInlineAdditionalProperties(Object param) {
        // TODO: Implement...
        
        
    }
    
    /**
     * test json serialization of form data
     *
     * 
     *
     */
    public void testJsonFormData(String param, String param2) {
        // TODO: Implement...
        
        
    }
    
}

