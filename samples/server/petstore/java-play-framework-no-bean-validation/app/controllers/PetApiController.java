package controllers;

import java.io.InputStream;
import apimodels.ModelApiResponse;
import apimodels.Pet;

import play.mvc.Controller;
import play.mvc.Result;
import play.mvc.Http;
import java.util.List;
import java.util.Map;
import java.util.ArrayList;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.JsonNode;
import com.google.inject.Inject;
import java.io.File;
import openapitools.OpenAPIUtils;
import com.fasterxml.jackson.core.type.TypeReference;


import openapitools.OpenAPIUtils.ApiAction;


public class PetApiController extends Controller {

    private final PetApiControllerImpInterface imp;
    private final ObjectMapper mapper;

    @Inject
    private PetApiController(PetApiControllerImpInterface imp) {
        this.imp = imp;
        mapper = new ObjectMapper();
    }


    @ApiAction
    public Result addPet() throws Exception {
        JsonNode nodepet = request().body().asJson();
        Pet pet;
        if (nodepet != null) {
            pet = mapper.readValue(nodepet.toString(), Pet.class);
        } else {
            throw new IllegalArgumentException("'Pet' parameter is required");
        }
        imp.addPet(pet);
        return ok();
    }

    @ApiAction
    public Result deletePet(Long petId) throws Exception {
        String valueapiKey = request().getHeader("api_key");
        String apiKey;
        if (valueapiKey != null) {
            apiKey = valueapiKey;
        } else {
            apiKey = null;
        }
        imp.deletePet(petId, apiKey);
        return ok();
    }

    @ApiAction
    public Result findPetsByStatus() throws Exception {
        String[] statusArray = request().queryString().get("status");
        if (statusArray == null) {
            throw new IllegalArgumentException("'status' parameter is required");
        }
        List<String> statusList = OpenAPIUtils.parametersToList("csv", statusArray);
        List<String> status = new ArrayList<String>();
        for (String curParam : statusList) {
            if (!curParam.isEmpty()) {
                //noinspection UseBulkOperation
                status.add(curParam);
            }
        }
        List<Pet> obj = imp.findPetsByStatus(status);
        JsonNode result = mapper.valueToTree(obj);
        return ok(result);
    }

    @ApiAction
    public Result findPetsByTags() throws Exception {
        String[] tagsArray = request().queryString().get("tags");
        if (tagsArray == null) {
            throw new IllegalArgumentException("'tags' parameter is required");
        }
        List<String> tagsList = OpenAPIUtils.parametersToList("csv", tagsArray);
        List<String> tags = new ArrayList<String>();
        for (String curParam : tagsList) {
            if (!curParam.isEmpty()) {
                //noinspection UseBulkOperation
                tags.add(curParam);
            }
        }
        List<Pet> obj = imp.findPetsByTags(tags);
        JsonNode result = mapper.valueToTree(obj);
        return ok(result);
    }

    @ApiAction
    public Result getPetById(Long petId) throws Exception {
        Pet obj = imp.getPetById(petId);
        JsonNode result = mapper.valueToTree(obj);
        return ok(result);
    }

    @ApiAction
    public Result updatePet() throws Exception {
        JsonNode nodepet = request().body().asJson();
        Pet pet;
        if (nodepet != null) {
            pet = mapper.readValue(nodepet.toString(), Pet.class);
        } else {
            throw new IllegalArgumentException("'Pet' parameter is required");
        }
        imp.updatePet(pet);
        return ok();
    }

    @ApiAction
    public Result updatePetWithForm(Long petId) throws Exception {
        String valuename = (request().body().asMultipartFormData().asFormUrlEncoded().get("name"))[0];
        String name;
        if (valuename != null) {
            name = valuename;
        } else {
            name = "null";
        }
        String valuestatus = (request().body().asMultipartFormData().asFormUrlEncoded().get("status"))[0];
        String status;
        if (valuestatus != null) {
            status = valuestatus;
        } else {
            status = "null";
        }
        imp.updatePetWithForm(petId, name, status);
        return ok();
    }

    @ApiAction
    public Result uploadFile(Long petId) throws Exception {
        String valueadditionalMetadata = (request().body().asMultipartFormData().asFormUrlEncoded().get("additionalMetadata"))[0];
        String additionalMetadata;
        if (valueadditionalMetadata != null) {
            additionalMetadata = valueadditionalMetadata;
        } else {
            additionalMetadata = "null";
        }
        Http.MultipartFormData.FilePart file = request().body().asMultipartFormData().getFile("file");
        ModelApiResponse obj = imp.uploadFile(petId, additionalMetadata, file);
        JsonNode result = mapper.valueToTree(obj);
        return ok(result);
    }
}
