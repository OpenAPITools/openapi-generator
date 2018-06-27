package org.openapitools.api;

import org.openapitools.api.*;
import org.openapitools.model.*;

import org.wso2.msf4j.formparam.FormDataParam;
import org.wso2.msf4j.formparam.FileInfo;

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

import javax.ws.rs.core.Response;
import javax.ws.rs.core.SecurityContext;


public abstract class FakeApiService {
    public abstract Response fakeOuterBooleanSerialize(Boolean booleanPostBody
 ) throws NotFoundException;
    public abstract Response fakeOuterCompositeSerialize(OuterComposite outerComposite
 ) throws NotFoundException;
    public abstract Response fakeOuterNumberSerialize(BigDecimal body
 ) throws NotFoundException;
    public abstract Response fakeOuterStringSerialize(String body
 ) throws NotFoundException;
    public abstract Response testBodyWithQueryParams(String query
 ,User user
 ) throws NotFoundException;
    public abstract Response testClientModel(Client client
 ) throws NotFoundException;
    public abstract Response testEndpointParameters(BigDecimal number
 ,Double _double
 ,String patternWithoutDelimiter
 ,byte[] _byte
 ,Integer integer
 ,Integer int32
 ,Long int64
 ,Float _float
 ,String string
 ,InputStream binaryInputStream, FileInfo binaryDetail
 ,Date date
 ,Date dateTime
 ,String password
 ,String paramCallback
 ) throws NotFoundException;
    public abstract Response testEnumParameters(List<String> enumHeaderStringArray
 ,String enumHeaderString
 ,List<String> enumQueryStringArray
 ,String enumQueryString
 ,Integer enumQueryInteger
 ,Double enumQueryDouble
 ,List<String> enumFormStringArray
 ,String enumFormString
 ) throws NotFoundException;
    public abstract Response testInlineAdditionalProperties(String requestBody
 ) throws NotFoundException;
    public abstract Response testJsonFormData(String param
 ,String param2
 ) throws NotFoundException;
}
