package org.openapitools.api;


import jakarta.ws.rs.*;

import org.jboss.resteasy.reactive.RestForm;
import org.jboss.resteasy.reactive.multipart.FileUpload;

import jakarta.validation.constraints.*;

@Path("/upload")
@jakarta.annotation.Generated(value = "org.openapitools.codegen.languages.JavaJAXRSSpecServerCodegen", comments = "Generator version: 7.26.0-SNAPSHOT")
public interface UploadApi {

    @POST
    @Consumes({ "multipart/form-data" })
    void uploadPost(
@RestForm(value = "file") FileUpload _file);

}
