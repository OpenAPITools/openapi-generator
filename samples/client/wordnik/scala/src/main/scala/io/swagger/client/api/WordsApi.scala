package io.swagger.client.api

import scala.collection.mutable.HashMap

class WordsApi(val defBasePath: String = "https://api.wordnik.com/v4",
               defApiInvoker: ApiInvoker = ApiInvoker) {
  var basePath = defBasePath
  var apiInvoker = defApiInvoker

  def addHeader(key: String, value: String) = apiInvoker.defaultHeaders += key -> value


  def getRandomWord(hasDictionaryDef: String, includePartOfSpeech: String, excludePartOfSpeech: String, minCorpusCount: Integer, maxCorpusCount: Integer, minDictionaryCount: Integer, maxDictionaryCount: Integer, minLength: Integer, maxLength: Integer): Option[WordObject] = {
    // create path and map variables
    val path = "/words.json/randomWord".replaceAll("\\{format\\}", "json")


    val contentType = {

      "application/json"
    }

    // query params
    val queryParams = new HashMap[String, String]
    val headerParams = new HashMap[String, String]



    if (String.valueOf(hasDictionaryDef) != "null") queryParams += "hasDictionaryDef" -> hasDictionaryDef.toString
    if (String.valueOf(includePartOfSpeech) != "null") queryParams += "includePartOfSpeech" -> includePartOfSpeech.toString
    if (String.valueOf(excludePartOfSpeech) != "null") queryParams += "excludePartOfSpeech" -> excludePartOfSpeech.toString
    if (String.valueOf(minCorpusCount) != "null") queryParams += "minCorpusCount" -> minCorpusCount.toString
    if (String.valueOf(maxCorpusCount) != "null") queryParams += "maxCorpusCount" -> maxCorpusCount.toString
    if (String.valueOf(minDictionaryCount) != "null") queryParams += "minDictionaryCount" -> minDictionaryCount.toString
    if (String.valueOf(maxDictionaryCount) != "null") queryParams += "maxDictionaryCount" -> maxDictionaryCount.toString
    if (String.valueOf(minLength) != "null") queryParams += "minLength" -> minLength.toString
    if (String.valueOf(maxLength) != "null") queryParams += "maxLength" -> maxLength.toString




    try {
      apiInvoker.invokeApi(basePath, path, "GET", queryParams.toMap, None, headerParams.toMap, contentType) match {
        case s: String =>
          Some(ApiInvoker.deserialize(s, "", classOf[WordObject]).asInstanceOf[WordObject])

        case _ => None
      }
    } catch {
      case ex: ApiException if ex.code == 404 => None
      case ex: ApiException => throw ex
    }
  }

  def getRandomWords(hasDictionaryDef: String, includePartOfSpeech: String, excludePartOfSpeech: String, minCorpusCount: Integer, maxCorpusCount: Integer, minDictionaryCount: Integer, maxDictionaryCount: Integer, minLength: Integer, maxLength: Integer, sortBy: String, sortOrder: String, limit: Integer) = {
    // create path and map variables
    val path = "/words.json/randomWords".replaceAll("\\{format\\}", "json")


    val contentType = {

      "application/json"
    }

    // query params
    val queryParams = new HashMap[String, String]
    val headerParams = new HashMap[String, String]



    if (String.valueOf(hasDictionaryDef) != "null") queryParams += "hasDictionaryDef" -> hasDictionaryDef.toString
    if (String.valueOf(includePartOfSpeech) != "null") queryParams += "includePartOfSpeech" -> includePartOfSpeech.toString
    if (String.valueOf(excludePartOfSpeech) != "null") queryParams += "excludePartOfSpeech" -> excludePartOfSpeech.toString
    if (String.valueOf(minCorpusCount) != "null") queryParams += "minCorpusCount" -> minCorpusCount.toString
    if (String.valueOf(maxCorpusCount) != "null") queryParams += "maxCorpusCount" -> maxCorpusCount.toString
    if (String.valueOf(minDictionaryCount) != "null") queryParams += "minDictionaryCount" -> minDictionaryCount.toString
    if (String.valueOf(maxDictionaryCount) != "null") queryParams += "maxDictionaryCount" -> maxDictionaryCount.toString
    if (String.valueOf(minLength) != "null") queryParams += "minLength" -> minLength.toString
    if (String.valueOf(maxLength) != "null") queryParams += "maxLength" -> maxLength.toString
    if (String.valueOf(sortBy) != "null") queryParams += "sortBy" -> sortBy.toString
    if (String.valueOf(sortOrder) != "null") queryParams += "sortOrder" -> sortOrder.toString
    if (String.valueOf(limit) != "null") queryParams += "limit" -> limit.toString




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

  def reverseDictionary(query: String, findSenseForWord: String, includeSourceDictionaries: String, excludeSourceDictionaries: String, includePartOfSpeech: String, excludePartOfSpeech: String, minCorpusCount: Integer, maxCorpusCount: Integer, minLength: Integer, maxLength: Integer, expandTerms: String, includeTags: String, sortBy: String, sortOrder: String, skip: String, limit: Integer): Option[DefinitionSearchResults] = {
    // create path and map variables
    val path = "/words.json/reverseDictionary".replaceAll("\\{format\\}", "json")


    val contentType = {

      "application/json"
    }

    // query params
    val queryParams = new HashMap[String, String]
    val headerParams = new HashMap[String, String]



    if (String.valueOf(query) != "null") queryParams += "query" -> query.toString
    if (String.valueOf(findSenseForWord) != "null") queryParams += "findSenseForWord" -> findSenseForWord.toString
    if (String.valueOf(includeSourceDictionaries) != "null") queryParams += "includeSourceDictionaries" -> includeSourceDictionaries.toString
    if (String.valueOf(excludeSourceDictionaries) != "null") queryParams += "excludeSourceDictionaries" -> excludeSourceDictionaries.toString
    if (String.valueOf(includePartOfSpeech) != "null") queryParams += "includePartOfSpeech" -> includePartOfSpeech.toString
    if (String.valueOf(excludePartOfSpeech) != "null") queryParams += "excludePartOfSpeech" -> excludePartOfSpeech.toString
    if (String.valueOf(minCorpusCount) != "null") queryParams += "minCorpusCount" -> minCorpusCount.toString
    if (String.valueOf(maxCorpusCount) != "null") queryParams += "maxCorpusCount" -> maxCorpusCount.toString
    if (String.valueOf(minLength) != "null") queryParams += "minLength" -> minLength.toString
    if (String.valueOf(maxLength) != "null") queryParams += "maxLength" -> maxLength.toString
    if (String.valueOf(expandTerms) != "null") queryParams += "expandTerms" -> expandTerms.toString
    if (String.valueOf(includeTags) != "null") queryParams += "includeTags" -> includeTags.toString
    if (String.valueOf(sortBy) != "null") queryParams += "sortBy" -> sortBy.toString
    if (String.valueOf(sortOrder) != "null") queryParams += "sortOrder" -> sortOrder.toString
    if (String.valueOf(skip) != "null") queryParams += "skip" -> skip.toString
    if (String.valueOf(limit) != "null") queryParams += "limit" -> limit.toString




    try {
      apiInvoker.invokeApi(basePath, path, "GET", queryParams.toMap, None, headerParams.toMap, contentType) match {
        case s: String =>
          Some(ApiInvoker.deserialize(s, "", classOf[DefinitionSearchResults]).asInstanceOf[DefinitionSearchResults])

        case _ => None
      }
    } catch {
      case ex: ApiException if ex.code == 404 => None
      case ex: ApiException => throw ex
    }
  }

  def searchWords(query: String, caseSensitive: String, includePartOfSpeech: String, excludePartOfSpeech: String, minCorpusCount: Integer, maxCorpusCount: Integer, minDictionaryCount: Integer, maxDictionaryCount: Integer, minLength: Integer, maxLength: Integer, skip: Integer, limit: Integer): Option[WordSearchResults] = {
    // create path and map variables
    val path = "/words.json/search/{query}".replaceAll("\\{format\\}", "json").replaceAll("\\{" + "query" + "\\}", apiInvoker.escape(query))




    val contentType = {

      "application/json"
    }

    // query params
    val queryParams = new HashMap[String, String]
    val headerParams = new HashMap[String, String]



    if (String.valueOf(caseSensitive) != "null") queryParams += "caseSensitive" -> caseSensitive.toString
    if (String.valueOf(includePartOfSpeech) != "null") queryParams += "includePartOfSpeech" -> includePartOfSpeech.toString
    if (String.valueOf(excludePartOfSpeech) != "null") queryParams += "excludePartOfSpeech" -> excludePartOfSpeech.toString
    if (String.valueOf(minCorpusCount) != "null") queryParams += "minCorpusCount" -> minCorpusCount.toString
    if (String.valueOf(maxCorpusCount) != "null") queryParams += "maxCorpusCount" -> maxCorpusCount.toString
    if (String.valueOf(minDictionaryCount) != "null") queryParams += "minDictionaryCount" -> minDictionaryCount.toString
    if (String.valueOf(maxDictionaryCount) != "null") queryParams += "maxDictionaryCount" -> maxDictionaryCount.toString
    if (String.valueOf(minLength) != "null") queryParams += "minLength" -> minLength.toString
    if (String.valueOf(maxLength) != "null") queryParams += "maxLength" -> maxLength.toString
    if (String.valueOf(skip) != "null") queryParams += "skip" -> skip.toString
    if (String.valueOf(limit) != "null") queryParams += "limit" -> limit.toString




    try {
      apiInvoker.invokeApi(basePath, path, "GET", queryParams.toMap, None, headerParams.toMap, contentType) match {
        case s: String =>
          Some(ApiInvoker.deserialize(s, "", classOf[WordSearchResults]).asInstanceOf[WordSearchResults])

        case _ => None
      }
    } catch {
      case ex: ApiException if ex.code == 404 => None
      case ex: ApiException => throw ex
    }
  }

  def getWordOfTheDay(date: String): Option[WordOfTheDay] = {
    // create path and map variables
    val path = "/words.json/wordOfTheDay".replaceAll("\\{format\\}", "json")


    val contentType = {

      "application/json"
    }

    // query params
    val queryParams = new HashMap[String, String]
    val headerParams = new HashMap[String, String]



    if (String.valueOf(date) != "null") queryParams += "date" -> date.toString




    try {
      apiInvoker.invokeApi(basePath, path, "GET", queryParams.toMap, None, headerParams.toMap, contentType) match {
        case s: String =>
          Some(ApiInvoker.deserialize(s, "", classOf[WordOfTheDay]).asInstanceOf[WordOfTheDay])

        case _ => None
      }
    } catch {
      case ex: ApiException if ex.code == 404 => None
      case ex: ApiException => throw ex
    }
  }

}
