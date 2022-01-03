package controllers;

import java.io.InputStream;
import apimodels.ModelApiResponse;
import apimodels.Pet;

import com.typesafe.config.Config;
import play.mvc.Controller;
import play.mvc.Result;
import play.mvc.Http;
import java.util.List;
import java.util.Map;
import java.util.ArrayList;
import java.util.LinkedHashSet;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.JsonNode;
import com.google.inject.Inject;
import java.io.File;
import play.libs.Files.TemporaryFile;
import openapitools.OpenAPIUtils;
import openapitools.SecurityAPIUtils;
import com.fasterxml.jackson.core.type.TypeReference;

import javax.validation.constraints.*;
import com.typesafe.config.Config;

import openapitools.OpenAPIUtils.ApiAction;

@javax.annotation.Generated(value = "org.openapitools.codegen.languages.JavaPlayFrameworkCodegen")
public class PetApiController extends Controller {
    private final PetApiControllerImp imp;
    private final ObjectMapper mapper;
    private final Config configuration;
    private final SecurityAPIUtils securityAPIUtils;

    @Inject
    private PetApiController(Config configuration, PetApiControllerImp imp, SecurityAPIUtils securityAPIUtils) {
        this.imp = imp;
        mapper = new ObjectMapper();
        this.configuration = configuration;
        this.securityAPIUtils = securityAPIUtils;
    }

    @ApiAction
    public Result addPet(Http.Request request) throws Exception {
        JsonNode nodebody = request.body().asJson();
        Pet body;
        if (nodebody != null) {
            body = mapper.readValue(nodebody.toString(), Pet.class);
            if (configuration.getBoolean("useInputBeanValidation")) {
                OpenAPIUtils.validate(body);
            }
        } else {
            throw new IllegalArgumentException("'body' parameter is required");
        }
                if (!securityAPIUtils.isRequestTokenValid(request, "petstore_auth")) {
            return unauthorized();
        }

        imp.addPet(request, body);
        return ok();

    }

    @ApiAction
    public Result deletePet(Http.Request request, Long petId) throws Exception {
        String valueapiKey = request.header("api_key").get();
        String apiKey;
        if (valueapiKey != null) {
            apiKey = valueapiKey;
        } else {
            apiKey = null;
        }
                if (!securityAPIUtils.isRequestTokenValid(request, "petstore_auth")) {
            return unauthorized();
        }

        imp.deletePet(request, petId, apiKey);
        return ok();

    }

    @ApiAction
    public Result findPetsByStatus(Http.Request request) throws Exception {
        String[] statusArray = request.queryString().get("status");
        if (statusArray == null) {
            throw new IllegalArgumentException("'status' parameter is required");
        }
        List<String> statusList = OpenAPIUtils.parametersToList("csv", statusArray);
        List<String> status = new ArrayList<>();
        for (String curParam : statusList) {
            if (!curParam.isEmpty()) {
                //noinspection UseBulkOperation
                status.add(curParam);
            }
        }
                if (!securityAPIUtils.isRequestTokenValid(request, "petstore_auth")) {
            return unauthorized();
        }

        List<Pet> obj = imp.findPetsByStatus(request, status);

        if (configuration.getBoolean("useOutputBeanValidation")) {
            for (Pet curItem : obj) {
                OpenAPIUtils.validate(curItem);
            }
        }

        JsonNode result = mapper.valueToTree(obj);

        return ok(result);

    }

    @ApiAction
    public Result findPetsByTags(Http.Request request) throws Exception {
        String[] tagsArray = request.queryString().get("tags");
        if (tagsArray == null) {
            throw new IllegalArgumentException("'tags' parameter is required");
        }
        List<String> tagsList = OpenAPIUtils.parametersToList("csv", tagsArray);
        List<String> tags = new ArrayList<>();
        for (String curParam : tagsList) {
            if (!curParam.isEmpty()) {
                //noinspection UseBulkOperation
                tags.add(curParam);
            }
        }
                if (!securityAPIUtils.isRequestTokenValid(request, "petstore_auth")) {
            return unauthorized();
        }

        List<Pet> obj = imp.findPetsByTags(request, tags);

        if (configuration.getBoolean("useOutputBeanValidation")) {
            for (Pet curItem : obj) {
                OpenAPIUtils.validate(curItem);
            }
        }

        JsonNode result = mapper.valueToTree(obj);

        return ok(result);

    }

    @ApiAction
    public Result getPetById(Http.Request request, Long petId) throws Exception {
                Pet obj = imp.getPetById(request, petId);

        if (configuration.getBoolean("useOutputBeanValidation")) {
            OpenAPIUtils.validate(obj);
        }

        JsonNode result = mapper.valueToTree(obj);

        return ok(result);

    }

    @ApiAction
    public Result updatePet(Http.Request request) throws Exception {
        JsonNode nodebody = request.body().asJson();
        Pet body;
        if (nodebody != null) {
            body = mapper.readValue(nodebody.toString(), Pet.class);
            if (configuration.getBoolean("useInputBeanValidation")) {
                OpenAPIUtils.validate(body);
            }
        } else {
            throw new IllegalArgumentException("'body' parameter is required");
        }
                if (!securityAPIUtils.isRequestTokenValid(request, "petstore_auth")) {
            return unauthorized();
        }

        imp.updatePet(request, body);
        return ok();

    }

    @ApiAction
    public Result updatePetWithForm(Http.Request request, Long petId) throws Exception {
        String valuename = (request.body().asMultipartFormData().asFormUrlEncoded().get("name"))[0];
        String name;
        if (valuename != null) {
            name = valuename;
        } else {
            name = null;
        }
        String valuestatus = (request.body().asMultipartFormData().asFormUrlEncoded().get("status"))[0];
        String status;
        if (valuestatus != null) {
            status = valuestatus;
        } else {
            status = null;
        }
                if (!securityAPIUtils.isRequestTokenValid(request, "petstore_auth")) {
            return unauthorized();
        }

        imp.updatePetWithForm(request, petId, name, status);
        return ok();

    }

    @ApiAction
    public Result uploadFile(Http.Request request, Long petId) throws Exception {
        String valueadditionalMetadata = (request.body().asMultipartFormData().asFormUrlEncoded().get("additionalMetadata"))[0];
        String additionalMetadata;
        if (valueadditionalMetadata != null) {
            additionalMetadata = valueadditionalMetadata;
        } else {
            additionalMetadata = null;
        }
        Http.MultipartFormData<TemporaryFile> bodyfile = request.body().asMultipartFormData();
        Http.MultipartFormData.FilePart<TemporaryFile> file = bodyfile.getFile("file");
                if (!securityAPIUtils.isRequestTokenValid(request, "petstore_auth")) {
            return unauthorized();
        }

        ModelApiResponse obj = imp.uploadFile(request, petId, additionalMetadata, file);

        if (configuration.getBoolean("useOutputBeanValidation")) {
            OpenAPIUtils.validate(obj);
        }

        JsonNode result = mapper.valueToTree(obj);

        return ok(result);

    }

}
