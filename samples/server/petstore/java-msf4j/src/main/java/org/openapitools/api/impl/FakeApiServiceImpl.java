package org.openapitools.api.impl;

import org.openapitools.api.*;
import org.openapitools.model.*;

import java.math.BigDecimal;
import org.openapitools.model.Client;
import java.util.Date;
import java.io.File;
import java.util.Map;
import org.openapitools.model.OuterComposite;
import org.openapitools.model.User;

import java.util.List;
import org.openapitools.api.NotFoundException;

import java.io.InputStream;

import org.wso2.msf4j.formparam.FormDataParam;
import org.wso2.msf4j.formparam.FileInfo;

import javax.ws.rs.core.Response;
import javax.ws.rs.core.SecurityContext;


public class FakeApiServiceImpl extends FakeApiService {
    @Override
    public Response fakeOuterBooleanSerialize(Boolean booleanPostBody
 ) throws NotFoundException {
        // do some magic!
        return Response.ok().entity(new ApiResponseMessage(ApiResponseMessage.OK, "magic!")).build();
    }
    @Override
    public Response fakeOuterCompositeSerialize(OuterComposite outerComposite
 ) throws NotFoundException {
        // do some magic!
        return Response.ok().entity(new ApiResponseMessage(ApiResponseMessage.OK, "magic!")).build();
    }
    @Override
    public Response fakeOuterNumberSerialize(BigDecimal body
 ) throws NotFoundException {
        // do some magic!
        return Response.ok().entity(new ApiResponseMessage(ApiResponseMessage.OK, "magic!")).build();
    }
    @Override
    public Response fakeOuterStringSerialize(String body
 ) throws NotFoundException {
        // do some magic!
        return Response.ok().entity(new ApiResponseMessage(ApiResponseMessage.OK, "magic!")).build();
    }
    @Override
    public Response testBodyWithQueryParams(String query
, User user
 ) throws NotFoundException {
        // do some magic!
        return Response.ok().entity(new ApiResponseMessage(ApiResponseMessage.OK, "magic!")).build();
    }
    @Override
    public Response testClientModel(Client client
 ) throws NotFoundException {
        // do some magic!
        return Response.ok().entity(new ApiResponseMessage(ApiResponseMessage.OK, "magic!")).build();
    }
    @Override
    public Response testEndpointParameters(BigDecimal number
, Double _double
, String patternWithoutDelimiter
, byte[] _byte
, Integer integer
, Integer int32
, Long int64
, Float _float
, String string
, InputStream binaryInputStream, FileInfo binaryDetail
, Date date
, Date dateTime
, String password
, String paramCallback
 ) throws NotFoundException {
        // do some magic!
        return Response.ok().entity(new ApiResponseMessage(ApiResponseMessage.OK, "magic!")).build();
    }
    @Override
    public Response testEnumParameters(List<String> enumHeaderStringArray
, String enumHeaderString
, List<String> enumQueryStringArray
, String enumQueryString
, Integer enumQueryInteger
, Double enumQueryDouble
, List<String> enumFormStringArray
, String enumFormString
 ) throws NotFoundException {
        // do some magic!
        return Response.ok().entity(new ApiResponseMessage(ApiResponseMessage.OK, "magic!")).build();
    }
    @Override
    public Response testInlineAdditionalProperties(String requestBody
 ) throws NotFoundException {
        // do some magic!
        return Response.ok().entity(new ApiResponseMessage(ApiResponseMessage.OK, "magic!")).build();
    }
    @Override
    public Response testJsonFormData(String param
, String param2
 ) throws NotFoundException {
        // do some magic!
        return Response.ok().entity(new ApiResponseMessage(ApiResponseMessage.OK, "magic!")).build();
    }
}
