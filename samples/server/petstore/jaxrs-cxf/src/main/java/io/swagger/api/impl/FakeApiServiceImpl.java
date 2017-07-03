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
    
    public Client testClientModel(Client body) {
        // TODO: Implement...
        
        return null;
    }
    
    public void testEndpointParameters(BigDecimal number, Double _double, String patternWithoutDelimiter, byte[] _byte, Integer integer, Integer int32, Long int64, Float _float, String string, byte[] binary, LocalDate date, Date dateTime, String password, String paramCallback) {
        // TODO: Implement...
        
        
    }
    
    public void testEnumParameters(List<String> enumFormStringArray, String enumFormString, List<String> enumHeaderStringArray, String enumHeaderString, List<String> enumQueryStringArray, String enumQueryString, Integer enumQueryInteger, Double enumQueryDouble) {
        // TODO: Implement...
        
        
    }
    
    public void testJsonFormData(String param, String param2) {
        // TODO: Implement...
        
        
    }
    
}

