package controllers;

import apimodels.Client;

import play.mvc.Http;
import java.util.List;
import java.util.ArrayList;
import java.util.HashMap;

import javax.validation.constraints.*;

@SuppressWarnings("RedundantThrows")
public interface FakeClassnameTags123ApiControllerImpInterface {
    default Result testClassnameHttp(Http.Request request, Client body) throws Exception {
        Client obj = testClassname(request, body);
        if (configuration.getBoolean("useOutputBeanValidation")) {
                OpenAPIUtils.validate(obj);
        }
        JsonNode result = mapper.valueToTree(obj);
        return ok(result);
    }

    Client testClassname(Http.Request request, Client body) throws Exception;

}
