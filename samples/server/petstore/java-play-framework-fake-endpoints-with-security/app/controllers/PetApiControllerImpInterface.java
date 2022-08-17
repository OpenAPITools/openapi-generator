package controllers;

import apimodels.Pet;

import com.google.inject.Inject;
import com.typesafe.config.Config;
import play.mvc.Controller;
import play.mvc.Http;
import java.util.List;
import java.util.ArrayList;
import java.util.HashMap;
import play.mvc.Result;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.JsonNode;
import openapitools.OpenAPIUtils;
import openapitools.SecurityAPIUtils;
import static play.mvc.Results.ok;
import static play.mvc.Results.unauthorized;
import play.libs.Files.TemporaryFile;

import javax.validation.constraints.*;

@SuppressWarnings("RedundantThrows")
public abstract class PetApiControllerImpInterface {
    @Inject private Config configuration;
    @Inject private SecurityAPIUtils securityAPIUtils;
    private ObjectMapper mapper = new ObjectMapper();

    public Result addPetHttp(Http.Request request, Pet body) throws Exception {
        if (!securityAPIUtils.isRequestTokenValid(request, "petstore_token")) {
            return unauthorized();
        }

        addPet(request, body);
        return ok();

    }

    public abstract void addPet(Http.Request request, Pet body) throws Exception;

    public Result findPetsByStatusHttp(Http.Request request, @NotNull List<String> status) throws Exception {
        List<Pet> obj = findPetsByStatus(request, status);

        if (configuration.getBoolean("useOutputBeanValidation")) {
            for (Pet curItem : obj) {
                OpenAPIUtils.validate(curItem);
            }
        }

        JsonNode result = mapper.valueToTree(obj);

        return ok(result);

    }

    public abstract List<Pet> findPetsByStatus(Http.Request request, @NotNull List<String> status) throws Exception;

    public Result updatePetHttp(Http.Request request, Pet body) throws Exception {
        updatePet(request, body);
        return ok();

    }

    public abstract void updatePet(Http.Request request, Pet body) throws Exception;

}
