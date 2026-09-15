package org.openapitools.api;

import java.io.File;
import org.jspecify.annotations.Nullable;

import jakarta.ws.rs.*;
import jakarta.ws.rs.core.Response;
import org.jboss.resteasy.reactive.ResponseStatus;



import org.jboss.resteasy.reactive.RestForm;
import org.jboss.resteasy.reactive.multipart.FileUpload;

import java.io.InputStream;
import java.util.Map;
import java.util.List;
import jakarta.validation.constraints.*;
import jakarta.validation.Valid;


@Path("/upload")
@jakarta.annotation.Generated(value = "org.openapitools.codegen.languages.JavaJAXRSSpecServerCodegen", comments = "Generator version: 7.26.0-SNAPSHOT")
public interface UploadApi {

    @POST
    @Consumes({ "multipart/form-data" })
    void uploadPost(@RestForm(value = "file") FileUpload _file);

}
