package org.openapitools.api;

import org.openapitools.model.Client;

import javax.ws.rs.*;
import javax.ws.rs.core.Response;




import java.io.InputStream;
import java.util.Map;
import java.util.List;
import javax.validation.constraints.*;
import javax.validation.Valid;

@org.eclipse.microprofile.openapi.annotations.OpenAPIDefinition(
   info = @org.eclipse.microprofile.openapi.annotations.info.Info(
        title = "", version="1.0.0", description="",
        license = @org.eclipse.microprofile.openapi.annotations.info.License(name = "Apache-2.0", url = "https://www.apache.org/licenses/LICENSE-2.0.html")
   ),
   tags = @org.eclipse.microprofile.openapi.annotations.tags.Tag(name="", description="")
)
@org.eclipse.microprofile.openapi.annotations.tags.Tag(name="", description="")
@org.eclipse.microprofile.openapi.annotations.security.SecuritySchemes(value = {
    @org.eclipse.microprofile.openapi.annotations.security.SecurityScheme(
         securitySchemeName = "petstore_auth",
         type = org.eclipse.microprofile.openapi.annotations.enums.SecuritySchemeType.OAUTH2,
         description = "",
         flows = @org.eclipse.microprofile.openapi.annotations.security.OAuthFlows(
            implicit = @org.eclipse.microprofile.openapi.annotations.security.OAuthFlow(authorizationUrl = "http://petstore.swagger.io/api/oauth/dialog",
            tokenUrl = "",
            refreshUrl = "",
            scopes = {
                @org.eclipse.microprofile.openapi.annotations.security.OAuthScope(name = "write:pets", description = "modify pets in your account"),
                @org.eclipse.microprofile.openapi.annotations.security.OAuthScope(name = "read:pets", description = "read your pets")
                 })) 
    ), @org.eclipse.microprofile.openapi.annotations.security.SecurityScheme(
         securitySchemeName = "api_key",
         type = org.eclipse.microprofile.openapi.annotations.enums.SecuritySchemeType.APIKEY,
         description = "",
         apiKeyName = "api_key",
         in = org.eclipse.microprofile.openapi.annotations.enums.SecuritySchemeIn.HEADER
    ), @org.eclipse.microprofile.openapi.annotations.security.SecurityScheme(
         securitySchemeName = "api_key_query",
         type = org.eclipse.microprofile.openapi.annotations.enums.SecuritySchemeType.APIKEY,
         description = "",
         apiKeyName = "api_key_query",
         in = org.eclipse.microprofile.openapi.annotations.enums.SecuritySchemeIn.QUERY
    ), @org.eclipse.microprofile.openapi.annotations.security.SecurityScheme(
         securitySchemeName = "http_basic_test",
         type = org.eclipse.microprofile.openapi.annotations.enums.SecuritySchemeType.HTTP,
         description = "",
         scheme = "basic"
    )
})
@Path("/fake_classname_test")
@javax.annotation.Generated(value = "org.openapitools.codegen.languages.JavaJAXRSSpecServerCodegen")
public class FakeClassnameTestApi {

    @PATCH
    @Consumes({ "application/json" })
    @Produces({ "application/json" })
    @org.eclipse.microprofile.openapi.annotations.security.SecurityRequirements(value={
             @org.eclipse.microprofile.openapi.annotations.security.SecurityRequirement(name = "api_key_query")
    })
    @org.eclipse.microprofile.openapi.annotations.Operation(operationId = "testClassname", summary = "To test class name in snake case", description = "To test class name in snake case")
    @org.eclipse.microprofile.openapi.annotations.tags.Tag(name="fake_classname_tags 123#$%^")
    @org.eclipse.microprofile.openapi.annotations.responses.APIResponses(value = { 
            @org.eclipse.microprofile.openapi.annotations.responses.APIResponse(responseCode = "200", description = "successful operation",  content = { 
                @org.eclipse.microprofile.openapi.annotations.media.Content(mediaType="application/json", schema = @org.eclipse.microprofile.openapi.annotations.media.Schema(implementation = Client.class))
            })
        })
    public Response testClassname(@Valid @NotNull Client body) {
        return Response.ok().entity("magic!").build();
    }
}
