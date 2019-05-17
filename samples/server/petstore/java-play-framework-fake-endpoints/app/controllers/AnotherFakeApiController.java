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
import openapitools.OpenAPIUtils;
import com.fasterxml.jackson.core.type.TypeReference;

import javax.validation.constraints.*;
import play.Configuration;

import openapitools.OpenAPIUtils.ApiAction;


public class AnotherFakeApiController extends Controller {

    private final AnotherFakeApiControllerImpInterface imp;
    private final ObjectMapper mapper;
    private final Configuration configuration;

    @Inject
    private AnotherFakeApiController(Configuration configuration, AnotherFakeApiControllerImpInterface imp) {
        this.imp = imp;
        mapper = new ObjectMapper();
        this.configuration = configuration;
    }


    @ApiAction
    public Result testSpecialTags() throws Exception {
        JsonNode nodeclient = request().body().asJson();
        Client client;
        if (nodeclient != null) {
            client = mapper.readValue(nodeclient.toString(), Client.class);
            if (configuration.getBoolean("useInputBeanValidation")) {
                OpenAPIUtils.validate(client);
            }
        } else {
            throw new IllegalArgumentException("'Client' parameter is required");
        }
        Client obj = imp.testSpecialTags(client);
        if (configuration.getBoolean("useOutputBeanValidation")) {
            OpenAPIUtils.validate(obj);
        }
        JsonNode result = mapper.valueToTree(obj);
        return ok(result);
    }
}
