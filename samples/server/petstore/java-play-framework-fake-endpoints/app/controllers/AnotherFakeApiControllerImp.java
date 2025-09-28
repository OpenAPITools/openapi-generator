package controllers;

import apimodels.Client;
import java.util.UUID;

import play.mvc.Http;
import java.util.List;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.LinkedHashSet;
import java.io.FileInputStream;
import play.libs.Files.TemporaryFile;
import javax.validation.constraints.*;
import javax.validation.Valid;
@javax.annotation.Generated(value = "org.openapitools.codegen.languages.JavaPlayFrameworkCodegen", comments = "Generator version: 7.17.0-SNAPSHOT")
public class AnotherFakeApiControllerImp extends AnotherFakeApiControllerImpInterface {
    @Override
    public Client call123testSpecialTags(Http.Request request, UUID uuidTest, Client body) throws Exception {
        //Do your magic!!!
        return new Client();
    }

}
