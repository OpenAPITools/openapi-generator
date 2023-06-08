package org.openapitools.api.impl;

import org.openapitools.api.*;
import java.math.BigDecimal;
import org.openapitools.model.Client;
import java.util.Date;
import java.io.File;
import org.openapitools.model.FileSchemaTestClass;
import org.joda.time.LocalDate;
import java.util.Map;
import org.openapitools.model.OuterComposite;
import org.openapitools.model.User;
import org.openapitools.model.XmlItem;

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
 * OpenAPI Petstore
 *
 * <p>This spec is mainly for testing Petstore server and contains fake endpoints, models. Please do not use this for any other purpose. Special characters: \" \\
 *
 */
public class FakeApiServiceImpl implements FakeApi {
    /**
     * creates an XmlItem
     *
     * this route creates an XmlItem
     *
     */
    public void createXmlItem(XmlItem xmlItem) {
        // TODO: Implement...

        
    }

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

    public void testBodyWithFileSchema(FileSchemaTestClass body) {
        // TODO: Implement...

        
    }

    public void testBodyWithQueryParams(String query, User body) {
        // TODO: Implement...

        
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
     * Fake endpoint for testing various parameters  假端點  偽のエンドポイント  가짜 엔드 포인트
     *
     * Fake endpoint for testing various parameters  假端點  偽のエンドポイント  가짜 엔드 포인트
     *
     */
    public void testEndpointParameters(BigDecimal number, Double _double, String patternWithoutDelimiter, byte[] _byte, Integer integer, Integer int32, Long int64, Float _float, String string,  Attachment binaryDetail, LocalDate date, Date dateTime, String password, String paramCallback) {
        // TODO: Implement...

        
    }

    /**
     * To test enum parameters
     *
     * To test enum parameters
     *
     */
    public void testEnumParameters(List<String> enumHeaderStringArray, List<String> enumQueryStringArray, String enumQueryString, Integer enumQueryInteger, Double enumQueryDouble, List<String> enumFormStringArray, String enumFormString) {
        // TODO: Implement...

        
    }

    /**
     * Fake endpoint to test group parameters (optional)
     *
     * Fake endpoint to test group parameters (optional)
     *
     */
    public void testGroupParameters(Integer requiredStringGroup, Boolean requiredBooleanGroup, Long requiredInt64Group, Integer stringGroup, Boolean booleanGroup, Long int64Group) {
        // TODO: Implement...

        
    }

    /**
     * test inline additionalProperties
     *
     */
    public void testInlineAdditionalProperties(Map<String, String> param) {
        // TODO: Implement...

        
    }

    /**
     * test json serialization of form data
     *
     */
    public void testJsonFormData(String param, String param2) {
        // TODO: Implement...

        
    }

    public void testQueryParameterCollectionFormat(List<String> pipe, List<String> ioutil, List<String> http, List<String> url, List<String> context) {
        // TODO: Implement...

        
    }

}
