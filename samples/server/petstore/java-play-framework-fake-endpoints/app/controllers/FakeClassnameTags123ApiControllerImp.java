package controllers;

import apimodels.Client;

import play.mvc.Http;
import java.util.List;
import java.util.ArrayList;
import java.util.HashMap;
import java.io.FileInputStream;
import javax.validation.constraints.*;

public class FakeClassnameTags123ApiControllerImp implements FakeClassnameTags123ApiControllerImpInterface {

    private final ObjectMapper mapper;

    @Inject
    private FakeClassnameTags123ApiControllerImp() {
        mapper = new ObjectMapper();
    }

    @Override
    public Client testClassname(Client body) throws Exception {
        //Do your magic!!!
        String accept = request().getHeader("Accept");
        if (accept != null && accept.contains("application/json")) {
            return mapper.readValue("", Client.class);
        }
        return new Client();
    }

}
