package io.swagger.client.api

import io.swagger.client.ApiInvoker
import io.swagger.client.ApiException

import java.io.File
import java.util.Date

import scala.collection.mutable.HashMap

class WordApi {
  var basePath: String = "http://api.wordnik.com/v4"
  var apiInvoker = ApiInvoker
  
  def addHeader(key: String, value: String) = apiInvoker.defaultHeaders += key -> value 

  
  def getWord (word: String, useCanonical: String, includeSuggestions: String)  = {
    // create path and map variables
    val path = "/word.json/{word}".replaceAll("\\{format\\}","json").replaceAll("\\{" + "word" + "\\}",apiInvoker.escape(word))

    

    
    val contentType = {
      
      "application/json"
    }

    // query params
    val queryParams = new HashMap[String, String]
    val headerParams = new HashMap[String, String]

    

    if(String.valueOf(useCanonical) != "null") queryParams += "useCanonical" -> useCanonical.toString
    if(String.valueOf(includeSuggestions) != "null") queryParams += "includeSuggestions" -> includeSuggestions.toString
    
    
    

    try {
      apiInvoker.invokeApi(basePath, path, "GET", queryParams.toMap, None, headerParams.toMap, contentType) match {
        case s: String =>
           
        case _ => None
      }
    } catch {
      case ex: ApiException if ex.code == 404 => None
      case ex: ApiException => throw ex
    }
  }
  
  def getAudio (word: String, useCanonical: String, limit: Integer)  = {
    // create path and map variables
    val path = "/word.json/{word}/audio".replaceAll("\\{format\\}","json").replaceAll("\\{" + "word" + "\\}",apiInvoker.escape(word))

    

    
    val contentType = {
      
      "application/json"
    }

    // query params
    val queryParams = new HashMap[String, String]
    val headerParams = new HashMap[String, String]

    

    if(String.valueOf(useCanonical) != "null") queryParams += "useCanonical" -> useCanonical.toString
    if(String.valueOf(limit) != "null") queryParams += "limit" -> limit.toString
    
    
    

    try {
      apiInvoker.invokeApi(basePath, path, "GET", queryParams.toMap, None, headerParams.toMap, contentType) match {
        case s: String =>
           
        case _ => None
      }
    } catch {
      case ex: ApiException if ex.code == 404 => None
      case ex: ApiException => throw ex
    }
  }
  
  def getDefinitions (word: String, limit: Integer, partOfSpeech: String, includeRelated: String, sourceDictionaries: List[String], useCanonical: String, includeTags: String)  = {
    // create path and map variables
    val path = "/word.json/{word}/definitions".replaceAll("\\{format\\}","json").replaceAll("\\{" + "word" + "\\}",apiInvoker.escape(word))

    

    
    val contentType = {
      
      "application/json"
    }

    // query params
    val queryParams = new HashMap[String, String]
    val headerParams = new HashMap[String, String]

    

    if(String.valueOf(limit) != "null") queryParams += "limit" -> limit.toString
    if(String.valueOf(partOfSpeech) != "null") queryParams += "partOfSpeech" -> partOfSpeech.toString
    if(String.valueOf(includeRelated) != "null") queryParams += "includeRelated" -> includeRelated.toString
    if(String.valueOf(sourceDictionaries) != "null") queryParams += "sourceDictionaries" -> sourceDictionaries.toString
    if(String.valueOf(useCanonical) != "null") queryParams += "useCanonical" -> useCanonical.toString
    if(String.valueOf(includeTags) != "null") queryParams += "includeTags" -> includeTags.toString
    
    
    

    try {
      apiInvoker.invokeApi(basePath, path, "GET", queryParams.toMap, None, headerParams.toMap, contentType) match {
        case s: String =>
           
        case _ => None
      }
    } catch {
      case ex: ApiException if ex.code == 404 => None
      case ex: ApiException => throw ex
    }
  }
  
  def getEtymologies (word: String, useCanonical: String)  = {
    // create path and map variables
    val path = "/word.json/{word}/etymologies".replaceAll("\\{format\\}","json").replaceAll("\\{" + "word" + "\\}",apiInvoker.escape(word))

    

    
    val contentType = {
      
      "application/json"
    }

    // query params
    val queryParams = new HashMap[String, String]
    val headerParams = new HashMap[String, String]

    

    if(String.valueOf(useCanonical) != "null") queryParams += "useCanonical" -> useCanonical.toString
    
    
    

    try {
      apiInvoker.invokeApi(basePath, path, "GET", queryParams.toMap, None, headerParams.toMap, contentType) match {
        case s: String =>
           
        case _ => None
      }
    } catch {
      case ex: ApiException if ex.code == 404 => None
      case ex: ApiException => throw ex
    }
  }
  
  def getExamples (word: String, includeDuplicates: String, useCanonical: String, skip: Integer, limit: Integer)  = {
    // create path and map variables
    val path = "/word.json/{word}/examples".replaceAll("\\{format\\}","json").replaceAll("\\{" + "word" + "\\}",apiInvoker.escape(word))

    

    
    val contentType = {
      
      "application/json"
    }

    // query params
    val queryParams = new HashMap[String, String]
    val headerParams = new HashMap[String, String]

    

    if(String.valueOf(includeDuplicates) != "null") queryParams += "includeDuplicates" -> includeDuplicates.toString
    if(String.valueOf(useCanonical) != "null") queryParams += "useCanonical" -> useCanonical.toString
    if(String.valueOf(skip) != "null") queryParams += "skip" -> skip.toString
    if(String.valueOf(limit) != "null") queryParams += "limit" -> limit.toString
    
    
    

    try {
      apiInvoker.invokeApi(basePath, path, "GET", queryParams.toMap, None, headerParams.toMap, contentType) match {
        case s: String =>
           
        case _ => None
      }
    } catch {
      case ex: ApiException if ex.code == 404 => None
      case ex: ApiException => throw ex
    }
  }
  
  def getWordFrequency (word: String, useCanonical: String, startYear: Integer, endYear: Integer)  = {
    // create path and map variables
    val path = "/word.json/{word}/frequency".replaceAll("\\{format\\}","json").replaceAll("\\{" + "word" + "\\}",apiInvoker.escape(word))

    

    
    val contentType = {
      
      "application/json"
    }

    // query params
    val queryParams = new HashMap[String, String]
    val headerParams = new HashMap[String, String]

    

    if(String.valueOf(useCanonical) != "null") queryParams += "useCanonical" -> useCanonical.toString
    if(String.valueOf(startYear) != "null") queryParams += "startYear" -> startYear.toString
    if(String.valueOf(endYear) != "null") queryParams += "endYear" -> endYear.toString
    
    
    

    try {
      apiInvoker.invokeApi(basePath, path, "GET", queryParams.toMap, None, headerParams.toMap, contentType) match {
        case s: String =>
           
        case _ => None
      }
    } catch {
      case ex: ApiException if ex.code == 404 => None
      case ex: ApiException => throw ex
    }
  }
  
  def getHyphenation (word: String, useCanonical: String, sourceDictionary: String, limit: Integer)  = {
    // create path and map variables
    val path = "/word.json/{word}/hyphenation".replaceAll("\\{format\\}","json").replaceAll("\\{" + "word" + "\\}",apiInvoker.escape(word))

    

    
    val contentType = {
      
      "application/json"
    }

    // query params
    val queryParams = new HashMap[String, String]
    val headerParams = new HashMap[String, String]

    

    if(String.valueOf(useCanonical) != "null") queryParams += "useCanonical" -> useCanonical.toString
    if(String.valueOf(sourceDictionary) != "null") queryParams += "sourceDictionary" -> sourceDictionary.toString
    if(String.valueOf(limit) != "null") queryParams += "limit" -> limit.toString
    
    
    

    try {
      apiInvoker.invokeApi(basePath, path, "GET", queryParams.toMap, None, headerParams.toMap, contentType) match {
        case s: String =>
           
        case _ => None
      }
    } catch {
      case ex: ApiException if ex.code == 404 => None
      case ex: ApiException => throw ex
    }
  }
  
  def getPhrases (word: String, limit: Integer, wlmi: Integer, useCanonical: String)  = {
    // create path and map variables
    val path = "/word.json/{word}/phrases".replaceAll("\\{format\\}","json").replaceAll("\\{" + "word" + "\\}",apiInvoker.escape(word))

    

    
    val contentType = {
      
      "application/json"
    }

    // query params
    val queryParams = new HashMap[String, String]
    val headerParams = new HashMap[String, String]

    

    if(String.valueOf(limit) != "null") queryParams += "limit" -> limit.toString
    if(String.valueOf(wlmi) != "null") queryParams += "wlmi" -> wlmi.toString
    if(String.valueOf(useCanonical) != "null") queryParams += "useCanonical" -> useCanonical.toString
    
    
    

    try {
      apiInvoker.invokeApi(basePath, path, "GET", queryParams.toMap, None, headerParams.toMap, contentType) match {
        case s: String =>
           
        case _ => None
      }
    } catch {
      case ex: ApiException if ex.code == 404 => None
      case ex: ApiException => throw ex
    }
  }
  
  def getTextPronunciations (word: String, useCanonical: String, sourceDictionary: String, typeFormat: String, limit: Integer)  = {
    // create path and map variables
    val path = "/word.json/{word}/pronunciations".replaceAll("\\{format\\}","json").replaceAll("\\{" + "word" + "\\}",apiInvoker.escape(word))

    

    
    val contentType = {
      
      "application/json"
    }

    // query params
    val queryParams = new HashMap[String, String]
    val headerParams = new HashMap[String, String]

    

    if(String.valueOf(useCanonical) != "null") queryParams += "useCanonical" -> useCanonical.toString
    if(String.valueOf(sourceDictionary) != "null") queryParams += "sourceDictionary" -> sourceDictionary.toString
    if(String.valueOf(typeFormat) != "null") queryParams += "typeFormat" -> typeFormat.toString
    if(String.valueOf(limit) != "null") queryParams += "limit" -> limit.toString
    
    
    

    try {
      apiInvoker.invokeApi(basePath, path, "GET", queryParams.toMap, None, headerParams.toMap, contentType) match {
        case s: String =>
           
        case _ => None
      }
    } catch {
      case ex: ApiException if ex.code == 404 => None
      case ex: ApiException => throw ex
    }
  }
  
  def getRelatedWords (word: String, useCanonical: String, relationshipTypes: String, limitPerRelationshipType: Integer)  = {
    // create path and map variables
    val path = "/word.json/{word}/relatedWords".replaceAll("\\{format\\}","json").replaceAll("\\{" + "word" + "\\}",apiInvoker.escape(word))

    

    
    val contentType = {
      
      "application/json"
    }

    // query params
    val queryParams = new HashMap[String, String]
    val headerParams = new HashMap[String, String]

    

    if(String.valueOf(useCanonical) != "null") queryParams += "useCanonical" -> useCanonical.toString
    if(String.valueOf(relationshipTypes) != "null") queryParams += "relationshipTypes" -> relationshipTypes.toString
    if(String.valueOf(limitPerRelationshipType) != "null") queryParams += "limitPerRelationshipType" -> limitPerRelationshipType.toString
    
    
    

    try {
      apiInvoker.invokeApi(basePath, path, "GET", queryParams.toMap, None, headerParams.toMap, contentType) match {
        case s: String =>
           
        case _ => None
      }
    } catch {
      case ex: ApiException if ex.code == 404 => None
      case ex: ApiException => throw ex
    }
  }
  
  def getTopExample (word: String, useCanonical: String)  = {
    // create path and map variables
    val path = "/word.json/{word}/topExample".replaceAll("\\{format\\}","json").replaceAll("\\{" + "word" + "\\}",apiInvoker.escape(word))

    

    
    val contentType = {
      
      "application/json"
    }

    // query params
    val queryParams = new HashMap[String, String]
    val headerParams = new HashMap[String, String]

    

    if(String.valueOf(useCanonical) != "null") queryParams += "useCanonical" -> useCanonical.toString
    
    
    

    try {
      apiInvoker.invokeApi(basePath, path, "GET", queryParams.toMap, None, headerParams.toMap, contentType) match {
        case s: String =>
           
        case _ => None
      }
    } catch {
      case ex: ApiException if ex.code == 404 => None
      case ex: ApiException => throw ex
    }
  }
  
}
