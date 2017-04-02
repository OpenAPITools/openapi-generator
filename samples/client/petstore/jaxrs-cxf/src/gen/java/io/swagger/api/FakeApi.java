package io.swagger.api;

import java.math.BigDecimal;
import io.swagger.model.Client;
import org.joda.time.LocalDate;

import java.io.InputStream;
import java.io.OutputStream;
import java.util.List;
import java.util.Map;
import javax.ws.rs.*;
import javax.ws.rs.core.Response;
import javax.ws.rs.core.MediaType;
import org.apache.cxf.jaxrs.ext.multipart.*;

import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import io.swagger.jaxrs.PATCH;

@Path("/")
@Api(value = "/", description = "")
public interface FakeApi  {

    @PATCH
    @Path("/fake")
    @Consumes({ "application/json" })
    @Produces({ "application/json" })
    @ApiOperation(value = "To test \"client\" model", tags={  })
    public Client testClientModel(Client body);

    @POST
    @Path("/fake")
    @Consumes({ "application/xml; charset=utf-8", "application/json; charset=utf-8" })
    @Produces({ "application/xml; charset=utf-8", "application/json; charset=utf-8" })
    @ApiOperation(value = "Fake endpoint for testing various parameters 假端點 偽のエンドポイント 가짜 엔드 포인트 ", tags={  })
    public void testEndpointParameters(@Multipart(value = "number")  BigDecimal number, @Multipart(value = "_double")  Double _double, @Multipart(value = "patternWithoutDelimiter")  String patternWithoutDelimiter, @Multipart(value = "_byte")  byte[] _byte, @Multipart(value = "integer", required = false)  Integer integer, @Multipart(value = "int32", required = false)  Integer int32, @Multipart(value = "int64", required = false)  Long int64, @Multipart(value = "_float", required = false)  Float _float, @Multipart(value = "string", required = false)  String string, @Multipart(value = "binary", required = false)  byte[] binary, @Multipart(value = "date", required = false)  LocalDate date, @Multipart(value = "dateTime", required = false)  javax.xml.datatype.XMLGregorianCalendar dateTime, @Multipart(value = "password", required = false)  String password, @Multipart(value = "paramCallback", required = false)  String paramCallback);

    @GET
    @Path("/fake")
    @Consumes({ "*/*" })
    @Produces({ "*/*" })
    @ApiOperation(value = "To test enum parameters", tags={  })
    public void testEnumParameters(@Multipart(value = "enumFormStringArray", required = false)  List<String> enumFormStringArray, @Multipart(value = "enumFormString", required = false)  String enumFormString, @HeaderParam("enum_header_string_array") List<String> enumHeaderStringArray, @HeaderParam("enum_header_string") String enumHeaderString, @QueryParam("enum_query_string_array")List<String> enumQueryStringArray, @QueryParam("enum_query_string")String enumQueryString, @QueryParam("enum_query_integer")Integer enumQueryInteger, @Multipart(value = "enumQueryDouble", required = false)  Double enumQueryDouble);
}

