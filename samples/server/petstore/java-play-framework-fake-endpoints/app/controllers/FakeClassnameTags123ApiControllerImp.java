package controllers;

import apimodels.Client;

import play.mvc.Http;
import javax.validation.constraints.*;
@javax.annotation.Generated(value = "org.openapitools.codegen.languages.JavaPlayFrameworkCodegen", comments = "Generator version: 7.26.0-SNAPSHOT")
public class FakeClassnameTags123ApiControllerImp extends FakeClassnameTags123ApiControllerImpInterface {
    @Override
    public Client testClassname(Http.Request request, Client body) throws Exception {
        //Do your magic!!!
        return new Client();
    }

}
