package io.swagger.api;

import io.swagger.api.*;
import io.swagger.model.*;

import org.wso2.msf4j.formparam.FormDataParam;
import org.wso2.msf4j.formparam.FileInfo;

import java.math.BigDecimal;
import io.swagger.model.Client;
import java.util.Date;
import io.swagger.model.OuterComposite;

import java.util.List;
import io.swagger.api.NotFoundException;

import java.io.InputStream;

import javax.ws.rs.core.Response;
import javax.ws.rs.core.SecurityContext;


public abstract class FakeApiService {
    public abstract Response fakeOuterBooleanSerialize(Boolean body
 ) throws NotFoundException;
    public abstract Response fakeOuterCompositeSerialize(OuterComposite body
 ) throws NotFoundException;
    public abstract Response fakeOuterNumberSerialize(BigDecimal body
 ) throws NotFoundException;
    public abstract Response fakeOuterStringSerialize(String body
 ) throws NotFoundException;
    public abstract Response testClientModel(Client body
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
 ,byte[] binary
 ,Date date
 ,Date dateTime
 ,String password
 ,String paramCallback
 ) throws NotFoundException;
    public abstract Response testEnumParameters(List<String> enumFormStringArray
 ,String enumFormString
 ,List<String> enumHeaderStringArray
 ,String enumHeaderString
 ,List<String> enumQueryStringArray
 ,String enumQueryString
 ,Integer enumQueryInteger
 ,Double enumQueryDouble
 ) throws NotFoundException;
    public abstract Response testInlineAdditionalProperties(Object param
 ) throws NotFoundException;
    public abstract Response testJsonFormData(String param
 ,String param2
 ) throws NotFoundException;
}
