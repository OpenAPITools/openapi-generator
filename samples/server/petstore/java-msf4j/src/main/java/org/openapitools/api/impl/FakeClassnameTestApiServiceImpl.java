package org.openapitools.api.impl;

import org.openapitools.api.*;
import org.openapitools.model.*;

import org.openapitools.model.Client;

import java.util.List;
import org.openapitools.api.NotFoundException;

import java.io.InputStream;

import org.wso2.msf4j.formparam.FormDataParam;
import org.wso2.msf4j.formparam.FileInfo;

import jakarta.ws.rs.core.Response;
import jakarta.ws.rs.core.SecurityContext;

@jakarta.annotation.Generated(value = "org.openapitools.codegen.languages.JavaMSF4JServerCodegen", comments = "Generator version: 7.5.0-SNAPSHOT")
public class FakeClassnameTestApiServiceImpl extends FakeClassnameTestApiService {
    @Override
    public Response testClassname(Client body
 ) throws NotFoundException {
        // do some magic!
        return Response.ok().entity(new ApiResponseMessage(ApiResponseMessage.OK, "magic!")).build();
    }
}
