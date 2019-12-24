package controllers;

import apimodels.Client;

import play.mvc.Http;
import java.util.List;
import java.util.ArrayList;
import java.util.HashMap;
import java.io.FileInputStream;
import javax.validation.constraints.*;

public class FakeClassnameTags123ApiControllerImp implements FakeClassnameTags123ApiControllerImpInterface {
    @Override
    public Client testClassname(Client body) throws Exception {
        //Do your magic!!!
        return new Client();
    }

}
