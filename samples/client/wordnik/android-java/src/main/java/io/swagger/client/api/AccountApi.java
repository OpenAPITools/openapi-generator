package io.swagger.client.api;

import io.swagger.client.ApiException;
import io.swagger.client.ApiInvoker;
import io.swagger.client.model.ApiTokenStatus;
import io.swagger.client.model.AuthenticationToken;
import io.swagger.client.model.User;
import io.swagger.client.model.WordList;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class AccountApi {
    String basePath = "https://api.wordnik.com/v4";
    ApiInvoker apiInvoker = ApiInvoker.getInstance();

    public void addHeader(String key, String value) {
        getInvoker().addDefaultHeader(key, value);
    }

    public ApiInvoker getInvoker() {
        return apiInvoker;
    }

    public String getBasePath() {
        return basePath;
    }

    public void setBasePath(String basePath) {
        this.basePath = basePath;
    }

    public ApiTokenStatus getApiTokenStatus(String api_key) throws ApiException {
        Object postBody = null;


        // create path and map variables
        String path = "/account.json/apiTokenStatus".replaceAll("\\{format\\}", "json");

        // query params
        Map<String, String> queryParams = new HashMap<String, String>();
        Map<String, String> headerParams = new HashMap<String, String>();


        headerParams.put("api_key", api_key);


        String contentType = "application/json";

        try {
            String response = apiInvoker.invokeAPI(basePath, path, "GET", queryParams, postBody, headerParams, contentType);
            if (response != null) {
                return (ApiTokenStatus) ApiInvoker.deserialize(response, "", ApiTokenStatus.class);
            } else {
                return null;
            }
        } catch (ApiException ex) {
            if (ex.getCode() == 404) {
                return null;
            } else {
                throw ex;
            }
        }
    }


    public AuthenticationToken authenticate(String username, String password) throws ApiException {
        Object postBody = null;


        // create path and map variables
        String path = "/account.json/authenticate/{username}".replaceAll("\\{format\\}", "json").replaceAll("\\{" + "username" + "\\}", apiInvoker.escapeString(username.toString()));

        // query params
        Map<String, String> queryParams = new HashMap<String, String>();
        Map<String, String> headerParams = new HashMap<String, String>();

        if (!"null".equals(String.valueOf(password))) {
            queryParams.put("password", String.valueOf(password));
        }


        String contentType = "application/json";

        try {
            String response = apiInvoker.invokeAPI(basePath, path, "GET", queryParams, postBody, headerParams, contentType);
            if (response != null) {
                return (AuthenticationToken) ApiInvoker.deserialize(response, "", AuthenticationToken.class);
            } else {
                return null;
            }
        } catch (ApiException ex) {
            if (ex.getCode() == 404) {
                return null;
            } else {
                throw ex;
            }
        }
    }


    public AuthenticationToken authenticatePost(String username, String body) throws ApiException {
        Object postBody = body;


        // create path and map variables
        String path = "/account.json/authenticate/{username}".replaceAll("\\{format\\}", "json").replaceAll("\\{" + "username" + "\\}", apiInvoker.escapeString(username.toString()));

        // query params
        Map<String, String> queryParams = new HashMap<String, String>();
        Map<String, String> headerParams = new HashMap<String, String>();


        String contentType = "application/json";

        try {
            String response = apiInvoker.invokeAPI(basePath, path, "POST", queryParams, postBody, headerParams, contentType);
            if (response != null) {
                return (AuthenticationToken) ApiInvoker.deserialize(response, "", AuthenticationToken.class);
            } else {
                return null;
            }
        } catch (ApiException ex) {
            if (ex.getCode() == 404) {
                return null;
            } else {
                throw ex;
            }
        }
    }


    public User getLoggedInUser(String auth_token) throws ApiException {
        Object postBody = null;


        // create path and map variables
        String path = "/account.json/user".replaceAll("\\{format\\}", "json");

        // query params
        Map<String, String> queryParams = new HashMap<String, String>();
        Map<String, String> headerParams = new HashMap<String, String>();


        headerParams.put("auth_token", auth_token);


        String contentType = "application/json";

        try {
            String response = apiInvoker.invokeAPI(basePath, path, "GET", queryParams, postBody, headerParams, contentType);
            if (response != null) {
                return (User) ApiInvoker.deserialize(response, "", User.class);
            } else {
                return null;
            }
        } catch (ApiException ex) {
            if (ex.getCode() == 404) {
                return null;
            } else {
                throw ex;
            }
        }
    }


    public List<WordList> getWordListsForLoggedInUser(String auth_token, Integer skip, Integer limit) throws ApiException {
        Object postBody = null;


        // create path and map variables
        String path = "/account.json/wordLists".replaceAll("\\{format\\}", "json");

        // query params
        Map<String, String> queryParams = new HashMap<String, String>();
        Map<String, String> headerParams = new HashMap<String, String>();

        if (!"null".equals(String.valueOf(skip))) {
            queryParams.put("skip", String.valueOf(skip));
        }
        if (!"null".equals(String.valueOf(limit))) {
            queryParams.put("limit", String.valueOf(limit));
        }


        headerParams.put("auth_token", auth_token);


        String contentType = "application/json";

        try {
            String response = apiInvoker.invokeAPI(basePath, path, "GET", queryParams, postBody, headerParams, contentType);
            if (response != null) {
                return (List<WordList>) ApiInvoker.deserialize(response, "array", WordList.class);
            } else {
                return null;
            }
        } catch (ApiException ex) {
            if (ex.getCode() == 404) {
                return null;
            } else {
                throw ex;
            }
        }
    }

}
