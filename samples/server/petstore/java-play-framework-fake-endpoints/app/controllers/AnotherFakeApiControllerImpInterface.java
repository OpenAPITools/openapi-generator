package controllers;

import apimodels.Client;

import play.mvc.Http;
import java.util.List;
import java.util.ArrayList;
import java.util.HashMap;

import javax.validation.constraints.*;

@SuppressWarnings("RedundantThrows")
public interface AnotherFakeApiControllerImpInterface {
    Client testSpecialTags(Client body) throws Exception;

}
