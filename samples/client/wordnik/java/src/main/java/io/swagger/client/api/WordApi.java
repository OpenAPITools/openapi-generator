package io.swagger.client.api;

import com.sun.jersey.multipart.FormDataMultiPart;
import io.swagger.client.ApiException;
import io.swagger.client.ApiInvoker;
import io.swagger.client.model.AudioFile;
import io.swagger.client.model.Bigram;
import io.swagger.client.model.Definition;
import io.swagger.client.model.Example;
import io.swagger.client.model.FrequencySummary;
import io.swagger.client.model.WordObject;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class WordApi {
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

    public WordObject getWord(String word, String useCanonical, String includeSuggestions) throws ApiException {
        Object postBody = null;


        // create path and map variables
        String path = "/word.json/{word}".replaceAll("\\{format\\}", "json")
                .replaceAll("\\{" + "word" + "\\}", apiInvoker.escapeString(word.toString()));

        // query params
        Map<String, String> queryParams = new HashMap<String, String>();
        Map<String, String> headerParams = new HashMap<String, String>();
        Map<String, String> formParams = new HashMap<String, String>();

        if (!"null".equals(String.valueOf(useCanonical))) {
            queryParams.put("useCanonical", String.valueOf(useCanonical));
        }
        if (!"null".equals(String.valueOf(includeSuggestions))) {
            queryParams.put("includeSuggestions", String.valueOf(includeSuggestions));
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


    public List<AudioFile> getAudio(String word, String useCanonical, Integer limit) throws ApiException {
        Object postBody = null;


        // create path and map variables
        String path = "/word.json/{word}/audio".replaceAll("\\{format\\}", "json")
                .replaceAll("\\{" + "word" + "\\}", apiInvoker.escapeString(word.toString()));

        // query params
        Map<String, String> queryParams = new HashMap<String, String>();
        Map<String, String> headerParams = new HashMap<String, String>();
        Map<String, String> formParams = new HashMap<String, String>();

        if (!"null".equals(String.valueOf(useCanonical))) {
            queryParams.put("useCanonical", String.valueOf(useCanonical));
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
                return (List<AudioFile>) ApiInvoker.deserialize(response, "array", AudioFile.class);
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


    public List<Definition> getDefinitions(String word, Integer limit, String partOfSpeech, String includeRelated, List<String> sourceDictionaries, String useCanonical, String includeTags) throws ApiException {
        Object postBody = null;


        // create path and map variables
        String path = "/word.json/{word}/definitions".replaceAll("\\{format\\}", "json")
                .replaceAll("\\{" + "word" + "\\}", apiInvoker.escapeString(word.toString()));

        // query params
        Map<String, String> queryParams = new HashMap<String, String>();
        Map<String, String> headerParams = new HashMap<String, String>();
        Map<String, String> formParams = new HashMap<String, String>();

        if (!"null".equals(String.valueOf(limit))) {
            queryParams.put("limit", String.valueOf(limit));
        }
        if (!"null".equals(String.valueOf(partOfSpeech))) {
            queryParams.put("partOfSpeech", String.valueOf(partOfSpeech));
        }
        if (!"null".equals(String.valueOf(includeRelated))) {
            queryParams.put("includeRelated", String.valueOf(includeRelated));
        }
        if (!"null".equals(String.valueOf(sourceDictionaries))) {
            queryParams.put("sourceDictionaries", String.valueOf(sourceDictionaries));
        }
        if (!"null".equals(String.valueOf(useCanonical))) {
            queryParams.put("useCanonical", String.valueOf(useCanonical));
        }
        if (!"null".equals(String.valueOf(includeTags))) {
            queryParams.put("includeTags", String.valueOf(includeTags));
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
                return (List<Definition>) ApiInvoker.deserialize(response, "array", Definition.class);
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


    public List<String> getEtymologies(String word, String useCanonical) throws ApiException {
        Object postBody = null;


        // create path and map variables
        String path = "/word.json/{word}/etymologies".replaceAll("\\{format\\}", "json")
                .replaceAll("\\{" + "word" + "\\}", apiInvoker.escapeString(word.toString()));

        // query params
        Map<String, String> queryParams = new HashMap<String, String>();
        Map<String, String> headerParams = new HashMap<String, String>();
        Map<String, String> formParams = new HashMap<String, String>();

        if (!"null".equals(String.valueOf(useCanonical))) {
            queryParams.put("useCanonical", String.valueOf(useCanonical));
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
                return (List<String>) ApiInvoker.deserialize(response, "array", String.class);
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


    public void getExamples(String word, String includeDuplicates, String useCanonical, Integer skip, Integer limit) throws ApiException {
        Object postBody = null;


        // create path and map variables
        String path = "/word.json/{word}/examples".replaceAll("\\{format\\}", "json")
                .replaceAll("\\{" + "word" + "\\}", apiInvoker.escapeString(word.toString()));

        // query params
        Map<String, String> queryParams = new HashMap<String, String>();
        Map<String, String> headerParams = new HashMap<String, String>();
        Map<String, String> formParams = new HashMap<String, String>();

        if (!"null".equals(String.valueOf(includeDuplicates))) {
            queryParams.put("includeDuplicates", String.valueOf(includeDuplicates));
        }
        if (!"null".equals(String.valueOf(useCanonical))) {
            queryParams.put("useCanonical", String.valueOf(useCanonical));
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


    public FrequencySummary getWordFrequency(String word, String useCanonical, Integer startYear, Integer endYear) throws ApiException {
        Object postBody = null;


        // create path and map variables
        String path = "/word.json/{word}/frequency".replaceAll("\\{format\\}", "json")
                .replaceAll("\\{" + "word" + "\\}", apiInvoker.escapeString(word.toString()));

        // query params
        Map<String, String> queryParams = new HashMap<String, String>();
        Map<String, String> headerParams = new HashMap<String, String>();
        Map<String, String> formParams = new HashMap<String, String>();

        if (!"null".equals(String.valueOf(useCanonical))) {
            queryParams.put("useCanonical", String.valueOf(useCanonical));
        }
        if (!"null".equals(String.valueOf(startYear))) {
            queryParams.put("startYear", String.valueOf(startYear));
        }
        if (!"null".equals(String.valueOf(endYear))) {
            queryParams.put("endYear", String.valueOf(endYear));
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
                return (FrequencySummary) ApiInvoker.deserialize(response, "", FrequencySummary.class);
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


    public void getHyphenation(String word, String useCanonical, String sourceDictionary, Integer limit) throws ApiException {
        Object postBody = null;


        // create path and map variables
        String path = "/word.json/{word}/hyphenation".replaceAll("\\{format\\}", "json")
                .replaceAll("\\{" + "word" + "\\}", apiInvoker.escapeString(word.toString()));

        // query params
        Map<String, String> queryParams = new HashMap<String, String>();
        Map<String, String> headerParams = new HashMap<String, String>();
        Map<String, String> formParams = new HashMap<String, String>();

        if (!"null".equals(String.valueOf(useCanonical))) {
            queryParams.put("useCanonical", String.valueOf(useCanonical));
        }
        if (!"null".equals(String.valueOf(sourceDictionary))) {
            queryParams.put("sourceDictionary", String.valueOf(sourceDictionary));
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


    public List<Bigram> getPhrases(String word, Integer limit, Integer wlmi, String useCanonical) throws ApiException {
        Object postBody = null;


        // create path and map variables
        String path = "/word.json/{word}/phrases".replaceAll("\\{format\\}", "json")
                .replaceAll("\\{" + "word" + "\\}", apiInvoker.escapeString(word.toString()));

        // query params
        Map<String, String> queryParams = new HashMap<String, String>();
        Map<String, String> headerParams = new HashMap<String, String>();
        Map<String, String> formParams = new HashMap<String, String>();

        if (!"null".equals(String.valueOf(limit))) {
            queryParams.put("limit", String.valueOf(limit));
        }
        if (!"null".equals(String.valueOf(wlmi))) {
            queryParams.put("wlmi", String.valueOf(wlmi));
        }
        if (!"null".equals(String.valueOf(useCanonical))) {
            queryParams.put("useCanonical", String.valueOf(useCanonical));
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
                return (List<Bigram>) ApiInvoker.deserialize(response, "array", Bigram.class);
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


    public void getTextPronunciations(String word, String useCanonical, String sourceDictionary, String typeFormat, Integer limit) throws ApiException {
        Object postBody = null;


        // create path and map variables
        String path = "/word.json/{word}/pronunciations".replaceAll("\\{format\\}", "json")
                .replaceAll("\\{" + "word" + "\\}", apiInvoker.escapeString(word.toString()));

        // query params
        Map<String, String> queryParams = new HashMap<String, String>();
        Map<String, String> headerParams = new HashMap<String, String>();
        Map<String, String> formParams = new HashMap<String, String>();

        if (!"null".equals(String.valueOf(useCanonical))) {
            queryParams.put("useCanonical", String.valueOf(useCanonical));
        }
        if (!"null".equals(String.valueOf(sourceDictionary))) {
            queryParams.put("sourceDictionary", String.valueOf(sourceDictionary));
        }
        if (!"null".equals(String.valueOf(typeFormat))) {
            queryParams.put("typeFormat", String.valueOf(typeFormat));
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


    public void getRelatedWords(String word, String useCanonical, String relationshipTypes, Integer limitPerRelationshipType) throws ApiException {
        Object postBody = null;


        // create path and map variables
        String path = "/word.json/{word}/relatedWords".replaceAll("\\{format\\}", "json")
                .replaceAll("\\{" + "word" + "\\}", apiInvoker.escapeString(word.toString()));

        // query params
        Map<String, String> queryParams = new HashMap<String, String>();
        Map<String, String> headerParams = new HashMap<String, String>();
        Map<String, String> formParams = new HashMap<String, String>();

        if (!"null".equals(String.valueOf(useCanonical))) {
            queryParams.put("useCanonical", String.valueOf(useCanonical));
        }
        if (!"null".equals(String.valueOf(relationshipTypes))) {
            queryParams.put("relationshipTypes", String.valueOf(relationshipTypes));
        }
        if (!"null".equals(String.valueOf(limitPerRelationshipType))) {
            queryParams.put("limitPerRelationshipType", String.valueOf(limitPerRelationshipType));
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


    public Example getTopExample(String word, String useCanonical) throws ApiException {
        Object postBody = null;


        // create path and map variables
        String path = "/word.json/{word}/topExample".replaceAll("\\{format\\}", "json")
                .replaceAll("\\{" + "word" + "\\}", apiInvoker.escapeString(word.toString()));

        // query params
        Map<String, String> queryParams = new HashMap<String, String>();
        Map<String, String> headerParams = new HashMap<String, String>();
        Map<String, String> formParams = new HashMap<String, String>();

        if (!"null".equals(String.valueOf(useCanonical))) {
            queryParams.put("useCanonical", String.valueOf(useCanonical));
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
                return (Example) ApiInvoker.deserialize(response, "", Example.class);
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
