package org.openapitools.api.impl;

import org.openapitools.api.*;
import java.math.BigDecimal;
import org.openapitools.model.Client;
import java.util.Date;
import java.io.File;
import org.openapitools.model.FileSchemaTestClass;
import java.util.List;
import org.joda.time.LocalDate;
import java.util.Map;
import org.openapitools.model.OuterComposite;
import org.openapitools.model.User;
import org.openapitools.model.XmlItem;

import java.io.InputStream;
import java.io.OutputStream;
import java.util.List;
import java.util.Map;
import java.io.File;
import com.fasterxml.jackson.databind.node.JsonNodeFactory;
import com.fasterxml.jackson.databind.node.ObjectNode;
import org.openapitools.codegen.utils.JsonCache;
import org.openapitools.codegen.utils.JsonCache.CacheException;
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
    private JsonCache cache;

    {
        try {
            File cacheFile = new File(System.getProperty("jaxrs.test.server.json",
                    "/Users/williamcheng/Code/openapi-generator/samples/server/petstore/jaxrs-cxf-test-data/src/main/resources/test-data.json"));
            cache = JsonCache.Factory.instance.get("test-data").load(cacheFile).child("/org.openapitools.api/FakeApi");
        } catch (CacheException e) {
            e.printStackTrace();
        }
    }

    /**
     * creates an XmlItem
     *
     * this route creates an XmlItem
     *
     */
    @Override
    public void createXmlItem(XmlItem xmlItem) {

    }

    @Override
    public Boolean fakeOuterBooleanSerialize(Boolean body) {
        try {
            Boolean response = cache.getBoolean("/fakeOuterBooleanSerialize/response");
            return response;
        } catch (CacheException e) {
            throw new RuntimeException(e);
        }
    }

    @Override
    public OuterComposite fakeOuterCompositeSerialize(OuterComposite body) {
        try {
            OuterComposite response = cache.getObject("/fakeOuterCompositeSerialize/response", OuterComposite.class);
            return response;
        } catch (CacheException e) {
            throw new RuntimeException(e);
        }
    }

    @Override
    public BigDecimal fakeOuterNumberSerialize(BigDecimal body) {
        try {
            BigDecimal response = cache.getBigDecimal("/fakeOuterNumberSerialize/response");
            return response;
        } catch (CacheException e) {
            throw new RuntimeException(e);
        }
    }

    @Override
    public String fakeOuterStringSerialize(String body) {
        try {
            String response = cache.getString("/fakeOuterStringSerialize/response");
            return response;
        } catch (CacheException e) {
            throw new RuntimeException(e);
        }
    }

    @Override
    public void testBodyWithFileSchema(FileSchemaTestClass body) {

    }

    @Override
    public void testBodyWithQueryParams(String query, User body) {

    }

    /**
     * To test \&quot;client\&quot; model
     *
     * To test \&quot;client\&quot; model
     *
     */
    @Override
    public Client testClientModel(Client body) {
        try {
            Client response = cache.getObject("/testClientModel/response", Client.class);
            return response;
        } catch (CacheException e) {
            throw new RuntimeException(e);
        }
    }

    /**
     * Fake endpoint for testing various parameters  假端點  偽のエンドポイント  가짜 엔드 포인트
     *
     * Fake endpoint for testing various parameters  假端點  偽のエンドポイント  가짜 엔드 포인트
     *
     */
    @Override
    public void testEndpointParameters(BigDecimal number, Double _double, String patternWithoutDelimiter, byte[] _byte, Integer integer, Integer int32, Long int64, Float _float, String string,  Attachment binaryDetail, LocalDate date, Date dateTime, String password, String paramCallback) {

    }

    /**
     * To test enum parameters
     *
     * To test enum parameters
     *
     */
    @Override
    public void testEnumParameters(List<String> enumHeaderStringArray, String enumHeaderString, List<String> enumQueryStringArray, String enumQueryString, Integer enumQueryInteger, Double enumQueryDouble, List<String> enumFormStringArray, String enumFormString) {

    }

    /**
     * Fake endpoint to test group parameters (optional)
     *
     * Fake endpoint to test group parameters (optional)
     *
     */
    @Override
    public void testGroupParameters(Integer requiredStringGroup, Boolean requiredBooleanGroup, Long requiredInt64Group, Integer stringGroup, Boolean booleanGroup, Long int64Group) {

    }

    /**
     * test inline additionalProperties
     *
     */
    @Override
    public void testInlineAdditionalProperties(Map<String, String> param) {

    }

    /**
     * test json serialization of form data
     *
     */
    @Override
    public void testJsonFormData(String param, String param2) {

    }

    @Override
    public void testQueryParameterCollectionFormat(List<String> pipe, List<String> ioutil, List<String> http, List<String> url, List<String> context) {

    }

}
