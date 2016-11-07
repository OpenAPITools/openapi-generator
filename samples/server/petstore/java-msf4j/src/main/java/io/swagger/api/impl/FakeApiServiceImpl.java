package io.swagger.api.impl;

import io.swagger.api.*;
import io.swagger.model.*;

import io.swagger.model.Client;
import java.util.Date;
import java.math.BigDecimal;

import java.util.List;
import io.swagger.api.NotFoundException;

import java.io.InputStream;

import org.wso2.msf4j.formparam.FormDataParam;
import org.wso2.msf4j.formparam.FileInfo;

import javax.ws.rs.core.Response;
import javax.ws.rs.core.SecurityContext;


public class FakeApiServiceImpl extends FakeApiService {
    @Override
    public Response testClientModel(Client body
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
, byte[] binary
, Date date
, Date dateTime
, String password
, String paramCallback
 ) throws NotFoundException {
        // do some magic!
        return Response.ok().entity(new ApiResponseMessage(ApiResponseMessage.OK, "magic!")).build();
    }
    @Override
    public Response testEnumParameters(List<String> enumFormStringArray
, String enumFormString
, List<String> enumHeaderStringArray
, String enumHeaderString
, List<String> enumQueryStringArray
, String enumQueryString
, BigDecimal enumQueryInteger
, Double enumQueryDouble
 ) throws NotFoundException {
        // do some magic!
        return Response.ok().entity(new ApiResponseMessage(ApiResponseMessage.OK, "magic!")).build();
    }
}
