package org.openapitools.api.impl;

import org.openapitools.api.*;
import org.openapitools.model.*;

import com.sun.jersey.multipart.FormDataParam;

import org.openapitools.model.XmlItem;

import java.util.List;
import org.openapitools.api.NotFoundException;

import java.io.InputStream;

import com.sun.jersey.core.header.FormDataContentDisposition;
import com.sun.jersey.multipart.FormDataParam;

import javax.ws.rs.core.Response;
import javax.ws.rs.core.SecurityContext;
import javax.validation.constraints.*;

public class DefaultApiServiceImpl extends DefaultApiService {
    @Override
    public Response createXmlItem(XmlItem xmlItem, SecurityContext securityContext)
    throws NotFoundException {
        // do some magic!
        return Response.ok().entity(new ApiResponseMessage(ApiResponseMessage.OK, "magic!")).build();
    }
}
