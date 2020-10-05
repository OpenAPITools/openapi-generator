package controllers;

import apimodels.Client;

import play.mvc.Http;
import java.util.List;
import java.util.ArrayList;
import java.util.HashMap;

import javax.validation.constraints.*;

@SuppressWarnings("RedundantThrows")
public interface AnotherFakeApiControllerImpInterface {
    default Result call123testSpecialTagsHttp(Http.Request request, Client body) throws Exception {
        Client obj = call123testSpecialTags(request, body);
        if (configuration.getBoolean("useOutputBeanValidation")) {
                OpenAPIUtils.validate(obj);
        }
        JsonNode result = mapper.valueToTree(obj);
        return ok(result);
    }

    Client call123testSpecialTags(Http.Request request, Client body) throws Exception;

}
