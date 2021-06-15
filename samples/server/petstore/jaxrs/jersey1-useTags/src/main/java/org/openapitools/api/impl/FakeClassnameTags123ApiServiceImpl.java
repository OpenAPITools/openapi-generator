package org.openapitools.api.impl;

import org.openapitools.api.*;
import org.openapitools.model.*;

import com.sun.jersey.multipart.FormDataParam;

import org.openapitools.model.Client;

import java.util.List;
import org.openapitools.api.NotFoundException;

import java.io.InputStream;

import com.sun.jersey.multipart.FormDataParam;
import com.sun.jersey.multipart.FormDataBodyPart;

import javax.ws.rs.core.Response;
import javax.ws.rs.core.SecurityContext;
import javax.validation.constraints.*;
@javax.annotation.Generated(value = "org.openapitools.codegen.languages.JavaJerseyServerCodegen")
public class FakeClassnameTags123ApiServiceImpl extends FakeClassnameTags123ApiService {
    @Override
    public Response testClassname(Client body, SecurityContext securityContext)
    throws NotFoundException {
        // do some magic!
        return Response.ok().entity(new ApiResponseMessage(ApiResponseMessage.OK, "magic!")).build();
    }
}
