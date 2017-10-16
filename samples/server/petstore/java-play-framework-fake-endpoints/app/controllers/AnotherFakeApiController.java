package controllers;

import apimodels.Client;

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


public class AnotherFakeApiController extends Controller {

    private final AnotherFakeApiControllerImpInterface imp;
    private final ObjectMapper mapper;

    @Inject
    private AnotherFakeApiController(AnotherFakeApiControllerImpInterface imp) {
        this.imp = imp;
        mapper = new ObjectMapper();
    }


    @ApiAction
    public Result testSpecialTags() throws Exception {
        JsonNode nodebody = request().body().asJson();
        Client body;
        if (nodebody != null) {
            body = mapper.readValue(nodebody.toString(), Client.class);
            body.validate();
        } else {
            throw new IllegalArgumentException("'body' parameter is required");
        }
        Client obj = imp.testSpecialTags(body);
        obj.validate();
        JsonNode result = mapper.valueToTree(obj);
        return ok(result);
    }
}
