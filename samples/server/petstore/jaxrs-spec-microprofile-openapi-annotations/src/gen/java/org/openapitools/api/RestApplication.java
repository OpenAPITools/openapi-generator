package org.openapitools.api;

import javax.ws.rs.ApplicationPath;
import javax.ws.rs.core.Application;

@org.eclipse.microprofile.openapi.annotations.OpenAPIDefinition(
    info = @org.eclipse.microprofile.openapi.annotations.info.Info(
        version="1.0.0"
        ,title = "OpenAPI Petstore"
        ,description = "This is a sample server Petstore server. For this sample, you can use the api key `special-key` to test the authorization filters."
        ,license = @org.eclipse.microprofile.openapi.annotations.info.License(name = "Apache-2.0", url = "https://www.apache.org/licenses/LICENSE-2.0.html")
        
))
@ApplicationPath(RestResourceRoot.APPLICATION_PATH)
public class RestApplication extends Application {

}
