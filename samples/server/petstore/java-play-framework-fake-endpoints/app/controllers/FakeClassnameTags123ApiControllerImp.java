package controllers;

import apimodels.Client;

import play.mvc.Http;
import java.util.List;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.LinkedHashSet;
import java.io.FileInputStream;
import play.libs.Files.TemporaryFile;
import javax.validation.constraints.*;
@javax.annotation.Generated(value = "org.openapitools.codegen.languages.JavaPlayFrameworkCodegen")
public class FakeClassnameTags123ApiControllerImp extends FakeClassnameTags123ApiControllerImpInterface {
    @Override
    public Client testClassname(Http.Request request, Client body) throws Exception {
        //Do your magic!!!
        return new Client();
    }

}
