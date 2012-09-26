package com.wordnik.client.api

import com.wordnik.client.model.Definition
import com.wordnik.client.model.TextPron
import com.wordnik.client.model.Example
import com.wordnik.client.model.Syllable
import com.wordnik.client.model.AudioFile
import com.wordnik.client.model.ExampleSearchResults
import com.wordnik.client.model.WordObject
import com.wordnik.client.model.Bigram
import com.wordnik.client.model.Related
import com.wordnik.client.model.FrequencySummary
import com.wordnik.client.common.ApiInvoker
import com.wordnik.client.common.ApiException
import scala.collection.mutable.HashMap

class WordApi {
  var basePath: String = "http://api.wordnik.com/v4"
  var apiInvoker = ApiInvoker
  
  def addHeader(key: String, value: String) = apiInvoker.defaultHeaders += key -> value 

  def getExamples (word: String, includeDuplicates: String= "false", useCanonical: String= "false", skip: Int= 0, limit: Int= 5) : Option[ExampleSearchResults]= {
    // create path and map variables
    val path = "/word.{format}/{word}/examples".replaceAll("\\{format\\}","json").replaceAll("\\{" + "word" + "\\}",apiInvoker.escapeString(word))

    // query params
    val queryParams = new HashMap[String, String]
    val headerParams = new HashMap[String, String]

    // verify required params are set
    (Set(word) - null).size match {
       case 1 => // all required values set
       case _ => throw new Exception("missing required params")
    }
    if(String.valueOf(includeDuplicates) != "null") queryParams += "includeDuplicates" -> includeDuplicates.toString
    if(String.valueOf(useCanonical) != "null") queryParams += "useCanonical" -> useCanonical.toString
    if(String.valueOf(skip) != "null") queryParams += "skip" -> skip.toString
    if(String.valueOf(limit) != "null") queryParams += "limit" -> limit.toString
    try {
      apiInvoker.invokeApi(basePath, path, "GET", queryParams.toMap, None, headerParams.toMap) match {
        case s: String =>
          Some(ApiInvoker.deserialize(s, "", classOf[ExampleSearchResults]).asInstanceOf[ExampleSearchResults])
        case _ => None
      }
    } catch {
      case ex: ApiException if ex.code == 404 => None
      case ex: ApiException => throw ex
    }
  }
  def getWord (word: String, useCanonical: String= "false", includeSuggestions: String= "true") : Option[WordObject]= {
    // create path and map variables
    val path = "/word.{format}/{word}".replaceAll("\\{format\\}","json").replaceAll("\\{" + "word" + "\\}",apiInvoker.escapeString(word))

    // query params
    val queryParams = new HashMap[String, String]
    val headerParams = new HashMap[String, String]

    // verify required params are set
    (Set(word) - null).size match {
       case 1 => // all required values set
       case _ => throw new Exception("missing required params")
    }
    if(String.valueOf(useCanonical) != "null") queryParams += "useCanonical" -> useCanonical.toString
    if(String.valueOf(includeSuggestions) != "null") queryParams += "includeSuggestions" -> includeSuggestions.toString
    try {
      apiInvoker.invokeApi(basePath, path, "GET", queryParams.toMap, None, headerParams.toMap) match {
        case s: String =>
          Some(ApiInvoker.deserialize(s, "", classOf[WordObject]).asInstanceOf[WordObject])
        case _ => None
      }
    } catch {
      case ex: ApiException if ex.code == 404 => None
      case ex: ApiException => throw ex
    }
  }
  def getDefinitions (word: String, partOfSpeech: String, sourceDictionaries: String, limit: Int= 200, includeRelated: String= "false", useCanonical: String= "false", includeTags: String= "false") : Option[List[Definition]]= {
    // create path and map variables
    val path = "/word.{format}/{word}/definitions".replaceAll("\\{format\\}","json").replaceAll("\\{" + "word" + "\\}",apiInvoker.escapeString(word))

    // query params
    val queryParams = new HashMap[String, String]
    val headerParams = new HashMap[String, String]

    // verify required params are set
    (Set(word) - null).size match {
       case 1 => // all required values set
       case _ => throw new Exception("missing required params")
    }
    if(String.valueOf(limit) != "null") queryParams += "limit" -> limit.toString
    if(String.valueOf(partOfSpeech) != "null") queryParams += "partOfSpeech" -> partOfSpeech.toString
    if(String.valueOf(includeRelated) != "null") queryParams += "includeRelated" -> includeRelated.toString
    if(String.valueOf(sourceDictionaries) != "null") queryParams += "sourceDictionaries" -> sourceDictionaries.toString
    if(String.valueOf(useCanonical) != "null") queryParams += "useCanonical" -> useCanonical.toString
    if(String.valueOf(includeTags) != "null") queryParams += "includeTags" -> includeTags.toString
    try {
      apiInvoker.invokeApi(basePath, path, "GET", queryParams.toMap, None, headerParams.toMap) match {
        case s: String =>
          Some(ApiInvoker.deserialize(s, "List", classOf[Definition]).asInstanceOf[List[Definition]])
        case _ => None
      }
    } catch {
      case ex: ApiException if ex.code == 404 => None
      case ex: ApiException => throw ex
    }
  }
  def getTopExample (word: String, useCanonical: String= "false") : Option[Example]= {
    // create path and map variables
    val path = "/word.{format}/{word}/topExample".replaceAll("\\{format\\}","json").replaceAll("\\{" + "word" + "\\}",apiInvoker.escapeString(word))

    // query params
    val queryParams = new HashMap[String, String]
    val headerParams = new HashMap[String, String]

    // verify required params are set
    (Set(word) - null).size match {
       case 1 => // all required values set
       case _ => throw new Exception("missing required params")
    }
    if(String.valueOf(useCanonical) != "null") queryParams += "useCanonical" -> useCanonical.toString
    try {
      apiInvoker.invokeApi(basePath, path, "GET", queryParams.toMap, None, headerParams.toMap) match {
        case s: String =>
          Some(ApiInvoker.deserialize(s, "", classOf[Example]).asInstanceOf[Example])
        case _ => None
      }
    } catch {
      case ex: ApiException if ex.code == 404 => None
      case ex: ApiException => throw ex
    }
  }
  def getRelatedWords (word: String, relationshipTypes: String, useCanonical: String= "false", limitPerRelationshipType: Int= 10) : Option[List[Related]]= {
    // create path and map variables
    val path = "/word.{format}/{word}/relatedWords".replaceAll("\\{format\\}","json").replaceAll("\\{" + "word" + "\\}",apiInvoker.escapeString(word))

    // query params
    val queryParams = new HashMap[String, String]
    val headerParams = new HashMap[String, String]

    // verify required params are set
    (Set(word) - null).size match {
       case 1 => // all required values set
       case _ => throw new Exception("missing required params")
    }
    if(String.valueOf(useCanonical) != "null") queryParams += "useCanonical" -> useCanonical.toString
    if(String.valueOf(relationshipTypes) != "null") queryParams += "relationshipTypes" -> relationshipTypes.toString
    if(String.valueOf(limitPerRelationshipType) != "null") queryParams += "limitPerRelationshipType" -> limitPerRelationshipType.toString
    try {
      apiInvoker.invokeApi(basePath, path, "GET", queryParams.toMap, None, headerParams.toMap) match {
        case s: String =>
          Some(ApiInvoker.deserialize(s, "List", classOf[Related]).asInstanceOf[List[Related]])
        case _ => None
      }
    } catch {
      case ex: ApiException if ex.code == 404 => None
      case ex: ApiException => throw ex
    }
  }
  def getTextPronunciations (word: String, sourceDictionary: String, typeFormat: String, useCanonical: String= "false", limit: Int= 50) : Option[List[TextPron]]= {
    // create path and map variables
    val path = "/word.{format}/{word}/pronunciations".replaceAll("\\{format\\}","json").replaceAll("\\{" + "word" + "\\}",apiInvoker.escapeString(word))

    // query params
    val queryParams = new HashMap[String, String]
    val headerParams = new HashMap[String, String]

    // verify required params are set
    (Set(word) - null).size match {
       case 1 => // all required values set
       case _ => throw new Exception("missing required params")
    }
    if(String.valueOf(useCanonical) != "null") queryParams += "useCanonical" -> useCanonical.toString
    if(String.valueOf(sourceDictionary) != "null") queryParams += "sourceDictionary" -> sourceDictionary.toString
    if(String.valueOf(typeFormat) != "null") queryParams += "typeFormat" -> typeFormat.toString
    if(String.valueOf(limit) != "null") queryParams += "limit" -> limit.toString
    try {
      apiInvoker.invokeApi(basePath, path, "GET", queryParams.toMap, None, headerParams.toMap) match {
        case s: String =>
          Some(ApiInvoker.deserialize(s, "List", classOf[TextPron]).asInstanceOf[List[TextPron]])
        case _ => None
      }
    } catch {
      case ex: ApiException if ex.code == 404 => None
      case ex: ApiException => throw ex
    }
  }
  def getHyphenation (word: String, sourceDictionary: String, useCanonical: String= "false", limit: Int= 50) : Option[List[Syllable]]= {
    // create path and map variables
    val path = "/word.{format}/{word}/hyphenation".replaceAll("\\{format\\}","json").replaceAll("\\{" + "word" + "\\}",apiInvoker.escapeString(word))

    // query params
    val queryParams = new HashMap[String, String]
    val headerParams = new HashMap[String, String]

    // verify required params are set
    (Set(word) - null).size match {
       case 1 => // all required values set
       case _ => throw new Exception("missing required params")
    }
    if(String.valueOf(useCanonical) != "null") queryParams += "useCanonical" -> useCanonical.toString
    if(String.valueOf(sourceDictionary) != "null") queryParams += "sourceDictionary" -> sourceDictionary.toString
    if(String.valueOf(limit) != "null") queryParams += "limit" -> limit.toString
    try {
      apiInvoker.invokeApi(basePath, path, "GET", queryParams.toMap, None, headerParams.toMap) match {
        case s: String =>
          Some(ApiInvoker.deserialize(s, "List", classOf[Syllable]).asInstanceOf[List[Syllable]])
        case _ => None
      }
    } catch {
      case ex: ApiException if ex.code == 404 => None
      case ex: ApiException => throw ex
    }
  }
  def getWordFrequency (word: String, useCanonical: String= "false", startYear: Int= 1800, endYear: Int= 2012) : Option[FrequencySummary]= {
    // create path and map variables
    val path = "/word.{format}/{word}/frequency".replaceAll("\\{format\\}","json").replaceAll("\\{" + "word" + "\\}",apiInvoker.escapeString(word))

    // query params
    val queryParams = new HashMap[String, String]
    val headerParams = new HashMap[String, String]

    // verify required params are set
    (Set(word) - null).size match {
       case 1 => // all required values set
       case _ => throw new Exception("missing required params")
    }
    if(String.valueOf(useCanonical) != "null") queryParams += "useCanonical" -> useCanonical.toString
    if(String.valueOf(startYear) != "null") queryParams += "startYear" -> startYear.toString
    if(String.valueOf(endYear) != "null") queryParams += "endYear" -> endYear.toString
    try {
      apiInvoker.invokeApi(basePath, path, "GET", queryParams.toMap, None, headerParams.toMap) match {
        case s: String =>
          Some(ApiInvoker.deserialize(s, "", classOf[FrequencySummary]).asInstanceOf[FrequencySummary])
        case _ => None
      }
    } catch {
      case ex: ApiException if ex.code == 404 => None
      case ex: ApiException => throw ex
    }
  }
  def getPhrases (word: String, limit: Int= 5, wlmi: Int= 0, useCanonical: String= "false") : Option[List[Bigram]]= {
    // create path and map variables
    val path = "/word.{format}/{word}/phrases".replaceAll("\\{format\\}","json").replaceAll("\\{" + "word" + "\\}",apiInvoker.escapeString(word))

    // query params
    val queryParams = new HashMap[String, String]
    val headerParams = new HashMap[String, String]

    // verify required params are set
    (Set(word) - null).size match {
       case 1 => // all required values set
       case _ => throw new Exception("missing required params")
    }
    if(String.valueOf(limit) != "null") queryParams += "limit" -> limit.toString
    if(String.valueOf(wlmi) != "null") queryParams += "wlmi" -> wlmi.toString
    if(String.valueOf(useCanonical) != "null") queryParams += "useCanonical" -> useCanonical.toString
    try {
      apiInvoker.invokeApi(basePath, path, "GET", queryParams.toMap, None, headerParams.toMap) match {
        case s: String =>
          Some(ApiInvoker.deserialize(s, "List", classOf[Bigram]).asInstanceOf[List[Bigram]])
        case _ => None
      }
    } catch {
      case ex: ApiException if ex.code == 404 => None
      case ex: ApiException => throw ex
    }
  }
  def getEtymologies (word: String, useCanonical: String) : Option[List[String]]= {
    // create path and map variables
    val path = "/word.{format}/{word}/etymologies".replaceAll("\\{format\\}","json").replaceAll("\\{" + "word" + "\\}",apiInvoker.escapeString(word))

    // query params
    val queryParams = new HashMap[String, String]
    val headerParams = new HashMap[String, String]

    // verify required params are set
    (Set(word) - null).size match {
       case 1 => // all required values set
       case _ => throw new Exception("missing required params")
    }
    if(String.valueOf(useCanonical) != "null") queryParams += "useCanonical" -> useCanonical.toString
    try {
      apiInvoker.invokeApi(basePath, path, "GET", queryParams.toMap, None, headerParams.toMap) match {
        case s: String =>
          Some(ApiInvoker.deserialize(s, "List", classOf[String]).asInstanceOf[List[String]])
        case _ => None
      }
    } catch {
      case ex: ApiException if ex.code == 404 => None
      case ex: ApiException => throw ex
    }
  }
  def getAudio (word: String, useCanonical: String= "false", limit: Int= 50) : Option[List[AudioFile]]= {
    // create path and map variables
    val path = "/word.{format}/{word}/audio".replaceAll("\\{format\\}","json").replaceAll("\\{" + "word" + "\\}",apiInvoker.escapeString(word))

    // query params
    val queryParams = new HashMap[String, String]
    val headerParams = new HashMap[String, String]

    // verify required params are set
    (Set(word) - null).size match {
       case 1 => // all required values set
       case _ => throw new Exception("missing required params")
    }
    if(String.valueOf(useCanonical) != "null") queryParams += "useCanonical" -> useCanonical.toString
    if(String.valueOf(limit) != "null") queryParams += "limit" -> limit.toString
    try {
      apiInvoker.invokeApi(basePath, path, "GET", queryParams.toMap, None, headerParams.toMap) match {
        case s: String =>
          Some(ApiInvoker.deserialize(s, "List", classOf[AudioFile]).asInstanceOf[List[AudioFile]])
        case _ => None
      }
    } catch {
      case ex: ApiException if ex.code == 404 => None
      case ex: ApiException => throw ex
    }
  }
  }

