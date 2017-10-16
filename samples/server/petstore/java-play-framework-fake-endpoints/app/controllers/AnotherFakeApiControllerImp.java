package controllers;

import apimodels.Client;

import play.mvc.Http;
import java.util.List;
import java.util.ArrayList;
import java.util.HashMap;
import java.io.FileInputStream;
import javax.validation.constraints.*;

public class AnotherFakeApiControllerImp implements AnotherFakeApiControllerImpInterface {
    @Override
    public Client testSpecialTags(Client body) throws Exception {
        //Do your magic!!!
        return new Client();
    }

}
