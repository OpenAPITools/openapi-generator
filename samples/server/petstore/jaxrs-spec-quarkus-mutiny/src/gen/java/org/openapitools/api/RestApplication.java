package org.openapitools.api;

import javax.ws.rs.ApplicationPath;
import javax.ws.rs.core.Application;

@org.eclipse.microprofile.openapi.annotations.OpenAPIDefinition(
    info = @org.eclipse.microprofile.openapi.annotations.info.Info(
        version="1.0.0"
        ,title = "OpenAPI Petstore"
        ,description = "This spec is mainly for testing Petstore server and contains fake endpoints, models. Please do not use this for any other purpose. Special characters: \" \\"
        ,license = @org.eclipse.microprofile.openapi.annotations.info.License(name = "Apache-2.0", url = "https://www.apache.org/licenses/LICENSE-2.0.html")
        
))
@ApplicationPath(RestResourceRoot.APPLICATION_PATH)
public class RestApplication extends Application {

}
