package com.wordnik.client.api;

import com.wordnik.client.common.ApiException;
import com.wordnik.client.common.ApiInvoker;
import com.wordnik.client.model.WordObject;
import com.wordnik.client.model.DefinitionSearchResults;
import com.wordnik.client.model.WordOfTheDay;
import com.wordnik.client.model.WordSearchResults;
import java.util.*;

public class WordsApi {
  String basePath = "http://api.wordnik.com/v4";
  ApiInvoker apiInvoker = ApiInvoker.getInstance();

  public ApiInvoker getInvoker() {
    return apiInvoker;
  }
  
  public void setBasePath(String basePath) {
    this.basePath = basePath;
  }
  
  public String getBasePath() {
    return basePath;
  }

  public WordSearchResults searchWords (String query, String includePartOfSpeech, String excludePartOfSpeech, String caseSensitive, Integer minCorpusCount, Integer maxCorpusCount, Integer minDictionaryCount, Integer maxDictionaryCount, Integer minLength, Integer maxLength, Integer skip, Integer limit) throws ApiException {
    // create path and map variables
    String path = "/words.{format}/search/{query}".replaceAll("\\{format\\}","json").replaceAll("\\{" + "query" + "\\}", apiInvoker.escapeString(query));

    // query params
    Map<String, String> queryParams = new HashMap<String, String>();
    Map<String, String> headerParams = new HashMap<String, String>();

    // verify required params are set
    if(query == null ) {
       throw new ApiException(400, "missing required params");
    }
    if(!"null".equals(String.valueOf(caseSensitive)))
      queryParams.put("caseSensitive", String.valueOf(caseSensitive));
    if(!"null".equals(String.valueOf(includePartOfSpeech)))
      queryParams.put("includePartOfSpeech", String.valueOf(includePartOfSpeech));
    if(!"null".equals(String.valueOf(excludePartOfSpeech)))
      queryParams.put("excludePartOfSpeech", String.valueOf(excludePartOfSpeech));
    if(!"null".equals(String.valueOf(minCorpusCount)))
      queryParams.put("minCorpusCount", String.valueOf(minCorpusCount));
    if(!"null".equals(String.valueOf(maxCorpusCount)))
      queryParams.put("maxCorpusCount", String.valueOf(maxCorpusCount));
    if(!"null".equals(String.valueOf(minDictionaryCount)))
      queryParams.put("minDictionaryCount", String.valueOf(minDictionaryCount));
    if(!"null".equals(String.valueOf(maxDictionaryCount)))
      queryParams.put("maxDictionaryCount", String.valueOf(maxDictionaryCount));
    if(!"null".equals(String.valueOf(minLength)))
      queryParams.put("minLength", String.valueOf(minLength));
    if(!"null".equals(String.valueOf(maxLength)))
      queryParams.put("maxLength", String.valueOf(maxLength));
    if(!"null".equals(String.valueOf(skip)))
      queryParams.put("skip", String.valueOf(skip));
    if(!"null".equals(String.valueOf(limit)))
      queryParams.put("limit", String.valueOf(limit));
    try {
      String response = apiInvoker.invokeAPI(basePath, path, "GET", queryParams, null, headerParams);
      if(response != null){
        return (WordSearchResults) ApiInvoker.deserialize(response, "", WordSearchResults.class);
      }
      else {
        return null;
      }
    } catch (ApiException ex) {
      if(ex.getCode() == 404) {
      	return null;
      }
      else {
        throw ex;
      }
    }
  }
  public WordOfTheDay getWordOfTheDay (String date) throws ApiException {
    // create path and map variables
    String path = "/words.{format}/wordOfTheDay".replaceAll("\\{format\\}","json");

    // query params
    Map<String, String> queryParams = new HashMap<String, String>();
    Map<String, String> headerParams = new HashMap<String, String>();

    if(!"null".equals(String.valueOf(date)))
      queryParams.put("date", String.valueOf(date));
    try {
      String response = apiInvoker.invokeAPI(basePath, path, "GET", queryParams, null, headerParams);
      if(response != null){
        return (WordOfTheDay) ApiInvoker.deserialize(response, "", WordOfTheDay.class);
      }
      else {
        return null;
      }
    } catch (ApiException ex) {
      if(ex.getCode() == 404) {
      	return null;
      }
      else {
        throw ex;
      }
    }
  }
  public DefinitionSearchResults reverseDictionary (String query, String findSenseForWord, String includeSourceDictionaries, String excludeSourceDictionaries, String includePartOfSpeech, String excludePartOfSpeech, String expandTerms, String sortBy, String sortOrder, Integer minCorpusCount, Integer maxCorpusCount, Integer minLength, Integer maxLength, String includeTags, String skip, Integer limit) throws ApiException {
    // create path and map variables
    String path = "/words.{format}/reverseDictionary".replaceAll("\\{format\\}","json");

    // query params
    Map<String, String> queryParams = new HashMap<String, String>();
    Map<String, String> headerParams = new HashMap<String, String>();

    // verify required params are set
    if(query == null ) {
       throw new ApiException(400, "missing required params");
    }
    if(!"null".equals(String.valueOf(query)))
      queryParams.put("query", String.valueOf(query));
    if(!"null".equals(String.valueOf(findSenseForWord)))
      queryParams.put("findSenseForWord", String.valueOf(findSenseForWord));
    if(!"null".equals(String.valueOf(includeSourceDictionaries)))
      queryParams.put("includeSourceDictionaries", String.valueOf(includeSourceDictionaries));
    if(!"null".equals(String.valueOf(excludeSourceDictionaries)))
      queryParams.put("excludeSourceDictionaries", String.valueOf(excludeSourceDictionaries));
    if(!"null".equals(String.valueOf(includePartOfSpeech)))
      queryParams.put("includePartOfSpeech", String.valueOf(includePartOfSpeech));
    if(!"null".equals(String.valueOf(excludePartOfSpeech)))
      queryParams.put("excludePartOfSpeech", String.valueOf(excludePartOfSpeech));
    if(!"null".equals(String.valueOf(minCorpusCount)))
      queryParams.put("minCorpusCount", String.valueOf(minCorpusCount));
    if(!"null".equals(String.valueOf(maxCorpusCount)))
      queryParams.put("maxCorpusCount", String.valueOf(maxCorpusCount));
    if(!"null".equals(String.valueOf(minLength)))
      queryParams.put("minLength", String.valueOf(minLength));
    if(!"null".equals(String.valueOf(maxLength)))
      queryParams.put("maxLength", String.valueOf(maxLength));
    if(!"null".equals(String.valueOf(expandTerms)))
      queryParams.put("expandTerms", String.valueOf(expandTerms));
    if(!"null".equals(String.valueOf(includeTags)))
      queryParams.put("includeTags", String.valueOf(includeTags));
    if(!"null".equals(String.valueOf(sortBy)))
      queryParams.put("sortBy", String.valueOf(sortBy));
    if(!"null".equals(String.valueOf(sortOrder)))
      queryParams.put("sortOrder", String.valueOf(sortOrder));
    if(!"null".equals(String.valueOf(skip)))
      queryParams.put("skip", String.valueOf(skip));
    if(!"null".equals(String.valueOf(limit)))
      queryParams.put("limit", String.valueOf(limit));
    try {
      String response = apiInvoker.invokeAPI(basePath, path, "GET", queryParams, null, headerParams);
      if(response != null){
        return (DefinitionSearchResults) ApiInvoker.deserialize(response, "", DefinitionSearchResults.class);
      }
      else {
        return null;
      }
    } catch (ApiException ex) {
      if(ex.getCode() == 404) {
      	return null;
      }
      else {
        throw ex;
      }
    }
  }
  public List<WordObject> getRandomWords (String includePartOfSpeech, String excludePartOfSpeech, String sortBy, String sortOrder, String hasDictionaryDef, Integer minCorpusCount, Integer maxCorpusCount, Integer minDictionaryCount, Integer maxDictionaryCount, Integer minLength, Integer maxLength, Integer limit) throws ApiException {
    // create path and map variables
    String path = "/words.{format}/randomWords".replaceAll("\\{format\\}","json");

    // query params
    Map<String, String> queryParams = new HashMap<String, String>();
    Map<String, String> headerParams = new HashMap<String, String>();

    if(!"null".equals(String.valueOf(hasDictionaryDef)))
      queryParams.put("hasDictionaryDef", String.valueOf(hasDictionaryDef));
    if(!"null".equals(String.valueOf(includePartOfSpeech)))
      queryParams.put("includePartOfSpeech", String.valueOf(includePartOfSpeech));
    if(!"null".equals(String.valueOf(excludePartOfSpeech)))
      queryParams.put("excludePartOfSpeech", String.valueOf(excludePartOfSpeech));
    if(!"null".equals(String.valueOf(minCorpusCount)))
      queryParams.put("minCorpusCount", String.valueOf(minCorpusCount));
    if(!"null".equals(String.valueOf(maxCorpusCount)))
      queryParams.put("maxCorpusCount", String.valueOf(maxCorpusCount));
    if(!"null".equals(String.valueOf(minDictionaryCount)))
      queryParams.put("minDictionaryCount", String.valueOf(minDictionaryCount));
    if(!"null".equals(String.valueOf(maxDictionaryCount)))
      queryParams.put("maxDictionaryCount", String.valueOf(maxDictionaryCount));
    if(!"null".equals(String.valueOf(minLength)))
      queryParams.put("minLength", String.valueOf(minLength));
    if(!"null".equals(String.valueOf(maxLength)))
      queryParams.put("maxLength", String.valueOf(maxLength));
    if(!"null".equals(String.valueOf(sortBy)))
      queryParams.put("sortBy", String.valueOf(sortBy));
    if(!"null".equals(String.valueOf(sortOrder)))
      queryParams.put("sortOrder", String.valueOf(sortOrder));
    if(!"null".equals(String.valueOf(limit)))
      queryParams.put("limit", String.valueOf(limit));
    try {
      String response = apiInvoker.invokeAPI(basePath, path, "GET", queryParams, null, headerParams);
      if(response != null){
        return (List<WordObject>) ApiInvoker.deserialize(response, "List", WordObject.class);
      }
      else {
        return null;
      }
    } catch (ApiException ex) {
      if(ex.getCode() == 404) {
      	return null;
      }
      else {
        throw ex;
      }
    }
  }
  public WordObject getRandomWord (String includePartOfSpeech, String excludePartOfSpeech, String hasDictionaryDef, Integer minCorpusCount, Integer maxCorpusCount, Integer minDictionaryCount, Integer maxDictionaryCount, Integer minLength, Integer maxLength) throws ApiException {
    // create path and map variables
    String path = "/words.{format}/randomWord".replaceAll("\\{format\\}","json");

    // query params
    Map<String, String> queryParams = new HashMap<String, String>();
    Map<String, String> headerParams = new HashMap<String, String>();

    if(!"null".equals(String.valueOf(hasDictionaryDef)))
      queryParams.put("hasDictionaryDef", String.valueOf(hasDictionaryDef));
    if(!"null".equals(String.valueOf(includePartOfSpeech)))
      queryParams.put("includePartOfSpeech", String.valueOf(includePartOfSpeech));
    if(!"null".equals(String.valueOf(excludePartOfSpeech)))
      queryParams.put("excludePartOfSpeech", String.valueOf(excludePartOfSpeech));
    if(!"null".equals(String.valueOf(minCorpusCount)))
      queryParams.put("minCorpusCount", String.valueOf(minCorpusCount));
    if(!"null".equals(String.valueOf(maxCorpusCount)))
      queryParams.put("maxCorpusCount", String.valueOf(maxCorpusCount));
    if(!"null".equals(String.valueOf(minDictionaryCount)))
      queryParams.put("minDictionaryCount", String.valueOf(minDictionaryCount));
    if(!"null".equals(String.valueOf(maxDictionaryCount)))
      queryParams.put("maxDictionaryCount", String.valueOf(maxDictionaryCount));
    if(!"null".equals(String.valueOf(minLength)))
      queryParams.put("minLength", String.valueOf(minLength));
    if(!"null".equals(String.valueOf(maxLength)))
      queryParams.put("maxLength", String.valueOf(maxLength));
    try {
      String response = apiInvoker.invokeAPI(basePath, path, "GET", queryParams, null, headerParams);
      if(response != null){
        return (WordObject) ApiInvoker.deserialize(response, "", WordObject.class);
      }
      else {
        return null;
      }
    } catch (ApiException ex) {
      if(ex.getCode() == 404) {
      	return null;
      }
      else {
        throw ex;
      }
    }
  }
  }

