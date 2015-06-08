package io.swagger.client.api;

import com.sun.jersey.multipart.FormDataMultiPart;
import io.swagger.client.ApiException;
import io.swagger.client.ApiInvoker;
import io.swagger.client.model.DefinitionSearchResults;
import io.swagger.client.model.WordObject;
import io.swagger.client.model.WordOfTheDay;
import io.swagger.client.model.WordSearchResults;

import java.util.HashMap;
import java.util.Map;

public class WordsApi {
    String basePath = "https://api.wordnik.com/v4";
    ApiInvoker apiInvoker = ApiInvoker.getInstance();

    public ApiInvoker getInvoker() {
        return apiInvoker;
    }

    public String getBasePath() {
        return basePath;
    }

    public void setBasePath(String basePath) {
        this.basePath = basePath;
    }

    public WordObject getRandomWord(String hasDictionaryDef, String includePartOfSpeech, String excludePartOfSpeech, Integer minCorpusCount, Integer maxCorpusCount, Integer minDictionaryCount, Integer maxDictionaryCount, Integer minLength, Integer maxLength) throws ApiException {
        Object postBody = null;


        // create path and map variables
        String path = "/words.json/randomWord".replaceAll("\\{format\\}", "json");

        // query params
        Map<String, String> queryParams = new HashMap<String, String>();
        Map<String, String> headerParams = new HashMap<String, String>();
        Map<String, String> formParams = new HashMap<String, String>();

        if (!"null".equals(String.valueOf(hasDictionaryDef))) {
            queryParams.put("hasDictionaryDef", String.valueOf(hasDictionaryDef));
        }
        if (!"null".equals(String.valueOf(includePartOfSpeech))) {
            queryParams.put("includePartOfSpeech", String.valueOf(includePartOfSpeech));
        }
        if (!"null".equals(String.valueOf(excludePartOfSpeech))) {
            queryParams.put("excludePartOfSpeech", String.valueOf(excludePartOfSpeech));
        }
        if (!"null".equals(String.valueOf(minCorpusCount))) {
            queryParams.put("minCorpusCount", String.valueOf(minCorpusCount));
        }
        if (!"null".equals(String.valueOf(maxCorpusCount))) {
            queryParams.put("maxCorpusCount", String.valueOf(maxCorpusCount));
        }
        if (!"null".equals(String.valueOf(minDictionaryCount))) {
            queryParams.put("minDictionaryCount", String.valueOf(minDictionaryCount));
        }
        if (!"null".equals(String.valueOf(maxDictionaryCount))) {
            queryParams.put("maxDictionaryCount", String.valueOf(maxDictionaryCount));
        }
        if (!"null".equals(String.valueOf(minLength))) {
            queryParams.put("minLength", String.valueOf(minLength));
        }
        if (!"null".equals(String.valueOf(maxLength))) {
            queryParams.put("maxLength", String.valueOf(maxLength));
        }


        String[] contentTypes = {

        };

        String contentType = contentTypes.length > 0 ? contentTypes[0] : "application/json";

        if (contentType.startsWith("multipart/form-data")) {
            boolean hasFields = false;
            FormDataMultiPart mp = new FormDataMultiPart();

            if (hasFields) {
                postBody = mp;
            }
        } else {

        }

        try {
            String response = apiInvoker.invokeAPI(basePath, path, "GET", queryParams, postBody, headerParams, formParams, contentType);
            if (response != null) {
                return (WordObject) ApiInvoker.deserialize(response, "", WordObject.class);
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


    public void getRandomWords(String hasDictionaryDef, String includePartOfSpeech, String excludePartOfSpeech, Integer minCorpusCount, Integer maxCorpusCount, Integer minDictionaryCount, Integer maxDictionaryCount, Integer minLength, Integer maxLength, String sortBy, String sortOrder, Integer limit) throws ApiException {
        Object postBody = null;


        // create path and map variables
        String path = "/words.json/randomWords".replaceAll("\\{format\\}", "json");

        // query params
        Map<String, String> queryParams = new HashMap<String, String>();
        Map<String, String> headerParams = new HashMap<String, String>();
        Map<String, String> formParams = new HashMap<String, String>();

        if (!"null".equals(String.valueOf(hasDictionaryDef))) {
            queryParams.put("hasDictionaryDef", String.valueOf(hasDictionaryDef));
        }
        if (!"null".equals(String.valueOf(includePartOfSpeech))) {
            queryParams.put("includePartOfSpeech", String.valueOf(includePartOfSpeech));
        }
        if (!"null".equals(String.valueOf(excludePartOfSpeech))) {
            queryParams.put("excludePartOfSpeech", String.valueOf(excludePartOfSpeech));
        }
        if (!"null".equals(String.valueOf(minCorpusCount))) {
            queryParams.put("minCorpusCount", String.valueOf(minCorpusCount));
        }
        if (!"null".equals(String.valueOf(maxCorpusCount))) {
            queryParams.put("maxCorpusCount", String.valueOf(maxCorpusCount));
        }
        if (!"null".equals(String.valueOf(minDictionaryCount))) {
            queryParams.put("minDictionaryCount", String.valueOf(minDictionaryCount));
        }
        if (!"null".equals(String.valueOf(maxDictionaryCount))) {
            queryParams.put("maxDictionaryCount", String.valueOf(maxDictionaryCount));
        }
        if (!"null".equals(String.valueOf(minLength))) {
            queryParams.put("minLength", String.valueOf(minLength));
        }
        if (!"null".equals(String.valueOf(maxLength))) {
            queryParams.put("maxLength", String.valueOf(maxLength));
        }
        if (!"null".equals(String.valueOf(sortBy))) {
            queryParams.put("sortBy", String.valueOf(sortBy));
        }
        if (!"null".equals(String.valueOf(sortOrder))) {
            queryParams.put("sortOrder", String.valueOf(sortOrder));
        }
        if (!"null".equals(String.valueOf(limit))) {
            queryParams.put("limit", String.valueOf(limit));
        }


        String[] contentTypes = {

        };

        String contentType = contentTypes.length > 0 ? contentTypes[0] : "application/json";

        if (contentType.startsWith("multipart/form-data")) {
            boolean hasFields = false;
            FormDataMultiPart mp = new FormDataMultiPart();

            if (hasFields) {
                postBody = mp;
            }
        } else {

        }

        try {
            String response = apiInvoker.invokeAPI(basePath, path, "GET", queryParams, postBody, headerParams, formParams, contentType);
            if (response != null) {
                return;
            } else {
                return;
            }
        } catch (ApiException ex) {
            if (ex.getCode() == 404) {
                return;
            } else {
                throw ex;
            }
        }
    }


    public DefinitionSearchResults reverseDictionary(String query, String findSenseForWord, String includeSourceDictionaries, String excludeSourceDictionaries, String includePartOfSpeech, String excludePartOfSpeech, Integer minCorpusCount, Integer maxCorpusCount, Integer minLength, Integer maxLength, String expandTerms, String includeTags, String sortBy, String sortOrder, String skip, Integer limit) throws ApiException {
        Object postBody = null;


        // create path and map variables
        String path = "/words.json/reverseDictionary".replaceAll("\\{format\\}", "json");

        // query params
        Map<String, String> queryParams = new HashMap<String, String>();
        Map<String, String> headerParams = new HashMap<String, String>();
        Map<String, String> formParams = new HashMap<String, String>();

        if (!"null".equals(String.valueOf(query))) {
            queryParams.put("query", String.valueOf(query));
        }
        if (!"null".equals(String.valueOf(findSenseForWord))) {
            queryParams.put("findSenseForWord", String.valueOf(findSenseForWord));
        }
        if (!"null".equals(String.valueOf(includeSourceDictionaries))) {
            queryParams.put("includeSourceDictionaries", String.valueOf(includeSourceDictionaries));
        }
        if (!"null".equals(String.valueOf(excludeSourceDictionaries))) {
            queryParams.put("excludeSourceDictionaries", String.valueOf(excludeSourceDictionaries));
        }
        if (!"null".equals(String.valueOf(includePartOfSpeech))) {
            queryParams.put("includePartOfSpeech", String.valueOf(includePartOfSpeech));
        }
        if (!"null".equals(String.valueOf(excludePartOfSpeech))) {
            queryParams.put("excludePartOfSpeech", String.valueOf(excludePartOfSpeech));
        }
        if (!"null".equals(String.valueOf(minCorpusCount))) {
            queryParams.put("minCorpusCount", String.valueOf(minCorpusCount));
        }
        if (!"null".equals(String.valueOf(maxCorpusCount))) {
            queryParams.put("maxCorpusCount", String.valueOf(maxCorpusCount));
        }
        if (!"null".equals(String.valueOf(minLength))) {
            queryParams.put("minLength", String.valueOf(minLength));
        }
        if (!"null".equals(String.valueOf(maxLength))) {
            queryParams.put("maxLength", String.valueOf(maxLength));
        }
        if (!"null".equals(String.valueOf(expandTerms))) {
            queryParams.put("expandTerms", String.valueOf(expandTerms));
        }
        if (!"null".equals(String.valueOf(includeTags))) {
            queryParams.put("includeTags", String.valueOf(includeTags));
        }
        if (!"null".equals(String.valueOf(sortBy))) {
            queryParams.put("sortBy", String.valueOf(sortBy));
        }
        if (!"null".equals(String.valueOf(sortOrder))) {
            queryParams.put("sortOrder", String.valueOf(sortOrder));
        }
        if (!"null".equals(String.valueOf(skip))) {
            queryParams.put("skip", String.valueOf(skip));
        }
        if (!"null".equals(String.valueOf(limit))) {
            queryParams.put("limit", String.valueOf(limit));
        }


        String[] contentTypes = {

        };

        String contentType = contentTypes.length > 0 ? contentTypes[0] : "application/json";

        if (contentType.startsWith("multipart/form-data")) {
            boolean hasFields = false;
            FormDataMultiPart mp = new FormDataMultiPart();

            if (hasFields) {
                postBody = mp;
            }
        } else {

        }

        try {
            String response = apiInvoker.invokeAPI(basePath, path, "GET", queryParams, postBody, headerParams, formParams, contentType);
            if (response != null) {
                return (DefinitionSearchResults) ApiInvoker.deserialize(response, "", DefinitionSearchResults.class);
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


    public WordSearchResults searchWords(String query, String caseSensitive, String includePartOfSpeech, String excludePartOfSpeech, Integer minCorpusCount, Integer maxCorpusCount, Integer minDictionaryCount, Integer maxDictionaryCount, Integer minLength, Integer maxLength, Integer skip, Integer limit) throws ApiException {
        Object postBody = null;


        // create path and map variables
        String path = "/words.json/search/{query}".replaceAll("\\{format\\}", "json")
                .replaceAll("\\{" + "query" + "\\}", apiInvoker.escapeString(query.toString()));

        // query params
        Map<String, String> queryParams = new HashMap<String, String>();
        Map<String, String> headerParams = new HashMap<String, String>();
        Map<String, String> formParams = new HashMap<String, String>();

        if (!"null".equals(String.valueOf(caseSensitive))) {
            queryParams.put("caseSensitive", String.valueOf(caseSensitive));
        }
        if (!"null".equals(String.valueOf(includePartOfSpeech))) {
            queryParams.put("includePartOfSpeech", String.valueOf(includePartOfSpeech));
        }
        if (!"null".equals(String.valueOf(excludePartOfSpeech))) {
            queryParams.put("excludePartOfSpeech", String.valueOf(excludePartOfSpeech));
        }
        if (!"null".equals(String.valueOf(minCorpusCount))) {
            queryParams.put("minCorpusCount", String.valueOf(minCorpusCount));
        }
        if (!"null".equals(String.valueOf(maxCorpusCount))) {
            queryParams.put("maxCorpusCount", String.valueOf(maxCorpusCount));
        }
        if (!"null".equals(String.valueOf(minDictionaryCount))) {
            queryParams.put("minDictionaryCount", String.valueOf(minDictionaryCount));
        }
        if (!"null".equals(String.valueOf(maxDictionaryCount))) {
            queryParams.put("maxDictionaryCount", String.valueOf(maxDictionaryCount));
        }
        if (!"null".equals(String.valueOf(minLength))) {
            queryParams.put("minLength", String.valueOf(minLength));
        }
        if (!"null".equals(String.valueOf(maxLength))) {
            queryParams.put("maxLength", String.valueOf(maxLength));
        }
        if (!"null".equals(String.valueOf(skip))) {
            queryParams.put("skip", String.valueOf(skip));
        }
        if (!"null".equals(String.valueOf(limit))) {
            queryParams.put("limit", String.valueOf(limit));
        }


        String[] contentTypes = {

        };

        String contentType = contentTypes.length > 0 ? contentTypes[0] : "application/json";

        if (contentType.startsWith("multipart/form-data")) {
            boolean hasFields = false;
            FormDataMultiPart mp = new FormDataMultiPart();

            if (hasFields) {
                postBody = mp;
            }
        } else {

        }

        try {
            String response = apiInvoker.invokeAPI(basePath, path, "GET", queryParams, postBody, headerParams, formParams, contentType);
            if (response != null) {
                return (WordSearchResults) ApiInvoker.deserialize(response, "", WordSearchResults.class);
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


    public WordOfTheDay getWordOfTheDay(String date) throws ApiException {
        Object postBody = null;


        // create path and map variables
        String path = "/words.json/wordOfTheDay".replaceAll("\\{format\\}", "json");

        // query params
        Map<String, String> queryParams = new HashMap<String, String>();
        Map<String, String> headerParams = new HashMap<String, String>();
        Map<String, String> formParams = new HashMap<String, String>();

        if (!"null".equals(String.valueOf(date))) {
            queryParams.put("date", String.valueOf(date));
        }


        String[] contentTypes = {

        };

        String contentType = contentTypes.length > 0 ? contentTypes[0] : "application/json";

        if (contentType.startsWith("multipart/form-data")) {
            boolean hasFields = false;
            FormDataMultiPart mp = new FormDataMultiPart();

            if (hasFields) {
                postBody = mp;
            }
        } else {

        }

        try {
            String response = apiInvoker.invokeAPI(basePath, path, "GET", queryParams, postBody, headerParams, formParams, contentType);
            if (response != null) {
                return (WordOfTheDay) ApiInvoker.deserialize(response, "", WordOfTheDay.class);
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
