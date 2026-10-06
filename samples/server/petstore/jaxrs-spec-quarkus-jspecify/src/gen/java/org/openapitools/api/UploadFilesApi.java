package org.openapitools.api;


import jakarta.ws.rs.*;

import org.jboss.resteasy.reactive.RestForm;
import org.jboss.resteasy.reactive.multipart.FileUpload;

import java.util.List;
import jakarta.validation.constraints.*;

@Path("/uploadFiles")
@jakarta.annotation.Generated(value = "org.openapitools.codegen.languages.JavaJAXRSSpecServerCodegen", comments = "Generator version: 7.26.0-SNAPSHOT")
public interface UploadFilesApi {

    @POST
    @Consumes({ "multipart/form-data" })
    void uploadFilesPost(
@RestForm(value = "file") List<FileUpload> _file);

}
