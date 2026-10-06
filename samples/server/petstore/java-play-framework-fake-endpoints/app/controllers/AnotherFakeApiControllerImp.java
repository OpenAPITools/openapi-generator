package controllers;

import apimodels.Client;
import java.util.UUID;

import play.mvc.Http;
import javax.validation.constraints.*;
@javax.annotation.Generated(value = "org.openapitools.codegen.languages.JavaPlayFrameworkCodegen", comments = "Generator version: 7.26.0-SNAPSHOT")
public class AnotherFakeApiControllerImp extends AnotherFakeApiControllerImpInterface {
    @Override
    public Client call123testSpecialTags(Http.Request request, UUID uuidTest, Client body) throws Exception {
        //Do your magic!!!
        return new Client();
    }

}
