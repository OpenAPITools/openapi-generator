package org.openapitools.api.impl;

import org.openapitools.api.*;
import org.openapitools.model.*;

import java.math.BigDecimal;
import org.openapitools.model.Client;
import java.io.File;
import java.time.LocalDate;
import java.util.Map;
import org.openapitools.model.ModelApiResponse;
import java.time.OffsetDateTime;
import org.openapitools.model.OuterComposite;
import org.openapitools.model.User;

import java.util.List;
import org.openapitools.api.NotFoundException;

import java.io.InputStream;

import org.glassfish.jersey.media.multipart.FormDataContentDisposition;

import javax.ws.rs.core.Response;
import javax.ws.rs.core.SecurityContext;
import javax.validation.constraints.*;

public class FakeApiServiceImpl extends FakeApiService {
    @Override
    public Response fakeOuterBooleanSerialize(Boolean body, SecurityContext securityContext) throws NotFoundException {
        // do some magic!
        return Response.ok().entity(new ApiResponseMessage(ApiResponseMessage.OK, "magic!")).build();
    }
    @Override
    public Response fakeOuterCompositeSerialize(OuterComposite outerComposite, SecurityContext securityContext) throws NotFoundException {
        // do some magic!
        return Response.ok().entity(new ApiResponseMessage(ApiResponseMessage.OK, "magic!")).build();
    }
    @Override
    public Response fakeOuterNumberSerialize(BigDecimal body, SecurityContext securityContext) throws NotFoundException {
        // do some magic!
        return Response.ok().entity(new ApiResponseMessage(ApiResponseMessage.OK, "magic!")).build();
    }
    @Override
    public Response fakeOuterStringSerialize(String body, SecurityContext securityContext) throws NotFoundException {
        // do some magic!
        return Response.ok().entity(new ApiResponseMessage(ApiResponseMessage.OK, "magic!")).build();
    }
    @Override
    public Response testBodyWithQueryParams( @NotNull String query, User user, SecurityContext securityContext) throws NotFoundException {
        // do some magic!
        return Response.ok().entity(new ApiResponseMessage(ApiResponseMessage.OK, "magic!")).build();
    }
    @Override
    public Response testClientModel(Client client, SecurityContext securityContext) throws NotFoundException {
        // do some magic!
        return Response.ok().entity(new ApiResponseMessage(ApiResponseMessage.OK, "magic!")).build();
    }
    @Override
    public Response testEndpointParameters(BigDecimal number, Double _double, String patternWithoutDelimiter, byte[] _byte, Integer integer, Integer int32, Long int64, Float _float, String string, InputStream binaryInputStream, FormDataContentDisposition binaryDetail, LocalDate date, OffsetDateTime dateTime, String password, String paramCallback, SecurityContext securityContext) throws NotFoundException {
        // do some magic!
        return Response.ok().entity(new ApiResponseMessage(ApiResponseMessage.OK, "magic!")).build();
    }
    @Override
    public Response testEnumParameters(List<String> enumHeaderStringArray, String enumHeaderString,  List<String> enumQueryStringArray,  String enumQueryString,  Integer enumQueryInteger,  Double enumQueryDouble, List<String> enumFormStringArray, String enumFormString, SecurityContext securityContext) throws NotFoundException {
        // do some magic!
        return Response.ok().entity(new ApiResponseMessage(ApiResponseMessage.OK, "magic!")).build();
    }
    @Override
    public Response testInlineAdditionalProperties(Map<String, String> requestBody, SecurityContext securityContext) throws NotFoundException {
        // do some magic!
        return Response.ok().entity(new ApiResponseMessage(ApiResponseMessage.OK, "magic!")).build();
    }
    @Override
    public Response testJsonFormData(String param, String param2, SecurityContext securityContext) throws NotFoundException {
        // do some magic!
        return Response.ok().entity(new ApiResponseMessage(ApiResponseMessage.OK, "magic!")).build();
    }
    @Override
    public Response uploadFileWithRequiredFile(Long petId, InputStream fileInputStream, FormDataContentDisposition fileDetail, String additionalMetadata, SecurityContext securityContext) throws NotFoundException {
        // do some magic!
        return Response.ok().entity(new ApiResponseMessage(ApiResponseMessage.OK, "magic!")).build();
    }
}
