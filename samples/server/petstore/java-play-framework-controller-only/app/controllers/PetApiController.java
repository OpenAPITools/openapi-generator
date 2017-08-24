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
import swagger.SwaggerUtils;
import com.fasterxml.jackson.core.type.TypeReference;

import javax.validation.constraints.*;

import swagger.SwaggerUtils.ApiAction;


public class PetApiController extends Controller {

    private final ObjectMapper mapper;

    @Inject
    private PetApiController() {
        mapper = new ObjectMapper();
    }


    @ApiAction
    public Result addPet() throws Exception {
        JsonNode nodebody = request().body().asJson();
        Pet body;

        body = mapper.readValue(nodebody.toString(), Pet.class);
        body.validate();

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
        return ok();
    }

    @ApiAction
    public Result findPetsByStatus() throws Exception {
        List<String> statusList = SwaggerUtils.parametersToList("csv", request().queryString().get("status"));
        List<String> status = new ArrayList<String>();
        for (String curParam : statusList) {
            //noinspection UseBulkOperation
            status.add(curParam);
        }
        return ok();
    }

    @ApiAction
    public Result findPetsByTags() throws Exception {
        List<String> tagsList = SwaggerUtils.parametersToList("csv", request().queryString().get("tags"));
        List<String> tags = new ArrayList<String>();
        for (String curParam : tagsList) {
            //noinspection UseBulkOperation
            tags.add(curParam);
        }
        return ok();
    }

    @ApiAction
    public Result getPetById(Long petId) throws Exception {
        return ok();
    }

    @ApiAction
    public Result updatePet() throws Exception {
        JsonNode nodebody = request().body().asJson();
        Pet body;

        body = mapper.readValue(nodebody.toString(), Pet.class);
        body.validate();

        return ok();
    }

    @ApiAction
    public Result updatePetWithForm(Long petId) throws Exception {
        String valuename = (request().body().asMultipartFormData().asFormUrlEncoded().get("name"))[0];
        String name;
        if (valuename != null) {
            name = valuename;
        
        } else {
            name = null;
        }
        String valuestatus = (request().body().asMultipartFormData().asFormUrlEncoded().get("status"))[0];
        String status;
        if (valuestatus != null) {
            status = valuestatus;
        
        } else {
            status = null;
        }
        return ok();
    }

    @ApiAction
    public Result uploadFile(Long petId) throws Exception {
        String valueadditionalMetadata = (request().body().asMultipartFormData().asFormUrlEncoded().get("additionalMetadata"))[0];
        String additionalMetadata;
        if (valueadditionalMetadata != null) {
            additionalMetadata = valueadditionalMetadata;
        
        } else {
            additionalMetadata = null;
        }
        Http.MultipartFormData.FilePart file = request().body().asMultipartFormData().getFile("file");
        return ok();
    }
}
