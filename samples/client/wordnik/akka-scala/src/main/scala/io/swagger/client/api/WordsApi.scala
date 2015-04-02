package io.swagger.client.api

import io.swagger.client.model.WordObject
import io.swagger.client.model.DefinitionSearchResults
import io.swagger.client.model.WordSearchResults
import io.swagger.client.model.WordOfTheDay
import io.swagger.client.core._
import io.swagger.client.core.CollectionFormats._
import io.swagger.client.core.ApiKeyLocations._

object WordsApi {

  /**
   * 
   * 
   * Expected answers:
   *   code 200 : WordObject (success)
   *   code 404 :  (No word found.)
   * 
   * @param HasDictionaryDef Only return words with dictionary definitions
   * @param IncludePartOfSpeech CSV part-of-speech values to include
   * @param ExcludePartOfSpeech CSV part-of-speech values to exclude
   * @param MinCorpusCount Minimum corpus frequency for terms
   * @param MaxCorpusCount Maximum corpus frequency for terms
   * @param MinDictionaryCount Minimum dictionary count
   * @param MaxDictionaryCount Maximum dictionary count
   * @param MinLength Minimum word length
   * @param MaxLength Maximum word length
   */
  def getRandomWord(HasDictionaryDef: Option[String] = None, IncludePartOfSpeech: Option[String] = None, ExcludePartOfSpeech: Option[String] = None, MinCorpusCount: Option[Int] = None, MaxCorpusCount: Option[Int] = None, MinDictionaryCount: Option[Int] = None, MaxDictionaryCount: Option[Int] = None, MinLength: Option[Int] = None, MaxLength: Option[Int] = None): ApiRequest[WordObject] =
    ApiRequest[WordObject](ApiMethods.GET, "https://api.wordnik.com/v4", "/words.json/randomWord", "application/json")
      .withQueryParam("hasDictionaryDef", HasDictionaryDef)
      .withQueryParam("includePartOfSpeech", IncludePartOfSpeech)
      .withQueryParam("excludePartOfSpeech", ExcludePartOfSpeech)
      .withQueryParam("minCorpusCount", MinCorpusCount)
      .withQueryParam("maxCorpusCount", MaxCorpusCount)
      .withQueryParam("minDictionaryCount", MinDictionaryCount)
      .withQueryParam("maxDictionaryCount", MaxDictionaryCount)
      .withQueryParam("minLength", MinLength)
      .withQueryParam("maxLength", MaxLength)
      .withSuccessResponse[WordObject](200)
      .withErrorResponse[Unit](404)
      
  /**
   * 
   * 
   * Expected answers:
   *   code 200 :  (success)
   *   code 400 :  (Invalid term supplied.)
   *   code 404 :  (No results.)
   * 
   * @param HasDictionaryDef Only return words with dictionary definitions
   * @param IncludePartOfSpeech CSV part-of-speech values to include
   * @param ExcludePartOfSpeech CSV part-of-speech values to exclude
   * @param MinCorpusCount Minimum corpus frequency for terms
   * @param MaxCorpusCount Maximum corpus frequency for terms
   * @param MinDictionaryCount Minimum dictionary count
   * @param MaxDictionaryCount Maximum dictionary count
   * @param MinLength Minimum word length
   * @param MaxLength Maximum word length
   * @param SortBy Attribute to sort by
   * @param SortOrder Sort direction
   * @param Limit Maximum number of results to return
   */
  def getRandomWords(HasDictionaryDef: Option[String] = None, IncludePartOfSpeech: Option[String] = None, ExcludePartOfSpeech: Option[String] = None, MinCorpusCount: Option[Int] = None, MaxCorpusCount: Option[Int] = None, MinDictionaryCount: Option[Int] = None, MaxDictionaryCount: Option[Int] = None, MinLength: Option[Int] = None, MaxLength: Option[Int] = None, SortBy: Option[String] = None, SortOrder: Option[String] = None, Limit: Option[Int] = None): ApiRequest[Unit] =
    ApiRequest[Unit](ApiMethods.GET, "https://api.wordnik.com/v4", "/words.json/randomWords", "application/json")
      .withQueryParam("hasDictionaryDef", HasDictionaryDef)
      .withQueryParam("includePartOfSpeech", IncludePartOfSpeech)
      .withQueryParam("excludePartOfSpeech", ExcludePartOfSpeech)
      .withQueryParam("minCorpusCount", MinCorpusCount)
      .withQueryParam("maxCorpusCount", MaxCorpusCount)
      .withQueryParam("minDictionaryCount", MinDictionaryCount)
      .withQueryParam("maxDictionaryCount", MaxDictionaryCount)
      .withQueryParam("minLength", MinLength)
      .withQueryParam("maxLength", MaxLength)
      .withQueryParam("sortBy", SortBy)
      .withQueryParam("sortOrder", SortOrder)
      .withQueryParam("limit", Limit)
      .withSuccessResponse[Unit](200)
      .withErrorResponse[Unit](400)
      .withErrorResponse[Unit](404)
      
  /**
   * 
   * 
   * Expected answers:
   *   code 200 : DefinitionSearchResults (success)
   *   code 400 :  (Invalid term supplied.)
   * 
   * @param Query Search term
   * @param FindSenseForWord Restricts words and finds closest sense
   * @param IncludeSourceDictionaries Only include these comma-delimited source dictionaries
   * @param ExcludeSourceDictionaries Exclude these comma-delimited source dictionaries
   * @param IncludePartOfSpeech Only include these comma-delimited parts of speech
   * @param ExcludePartOfSpeech Exclude these comma-delimited parts of speech
   * @param MinCorpusCount Minimum corpus frequency for terms
   * @param MaxCorpusCount Maximum corpus frequency for terms
   * @param MinLength Minimum word length
   * @param MaxLength Maximum word length
   * @param ExpandTerms Expand terms
   * @param IncludeTags Return a closed set of XML tags in response
   * @param SortBy Attribute to sort by
   * @param SortOrder Sort direction
   * @param Skip Results to skip
   * @param Limit Maximum number of results to return
   */
  def reverseDictionary(Query: String, FindSenseForWord: Option[String] = None, IncludeSourceDictionaries: Option[String] = None, ExcludeSourceDictionaries: Option[String] = None, IncludePartOfSpeech: Option[String] = None, ExcludePartOfSpeech: Option[String] = None, MinCorpusCount: Option[Int] = None, MaxCorpusCount: Option[Int] = None, MinLength: Option[Int] = None, MaxLength: Option[Int] = None, ExpandTerms: Option[String] = None, IncludeTags: Option[String] = None, SortBy: Option[String] = None, SortOrder: Option[String] = None, Skip: Option[String] = None, Limit: Option[Int] = None): ApiRequest[DefinitionSearchResults] =
    ApiRequest[DefinitionSearchResults](ApiMethods.GET, "https://api.wordnik.com/v4", "/words.json/reverseDictionary", "application/json")
      .withQueryParam("query", Query)
      .withQueryParam("findSenseForWord", FindSenseForWord)
      .withQueryParam("includeSourceDictionaries", IncludeSourceDictionaries)
      .withQueryParam("excludeSourceDictionaries", ExcludeSourceDictionaries)
      .withQueryParam("includePartOfSpeech", IncludePartOfSpeech)
      .withQueryParam("excludePartOfSpeech", ExcludePartOfSpeech)
      .withQueryParam("minCorpusCount", MinCorpusCount)
      .withQueryParam("maxCorpusCount", MaxCorpusCount)
      .withQueryParam("minLength", MinLength)
      .withQueryParam("maxLength", MaxLength)
      .withQueryParam("expandTerms", ExpandTerms)
      .withQueryParam("includeTags", IncludeTags)
      .withQueryParam("sortBy", SortBy)
      .withQueryParam("sortOrder", SortOrder)
      .withQueryParam("skip", Skip)
      .withQueryParam("limit", Limit)
      .withSuccessResponse[DefinitionSearchResults](200)
      .withErrorResponse[Unit](400)
      
  /**
   * 
   * 
   * Expected answers:
   *   code 200 : WordSearchResults (success)
   *   code 400 :  (Invalid query supplied.)
   * 
   * @param Query Search query
   * @param CaseSensitive Search case sensitive
   * @param IncludePartOfSpeech Only include these comma-delimited parts of speech
   * @param ExcludePartOfSpeech Exclude these comma-delimited parts of speech
   * @param MinCorpusCount Minimum corpus frequency for terms
   * @param MaxCorpusCount Maximum corpus frequency for terms
   * @param MinDictionaryCount Minimum number of dictionary entries for words returned
   * @param MaxDictionaryCount Maximum dictionary definition count
   * @param MinLength Minimum word length
   * @param MaxLength Maximum word length
   * @param Skip Results to skip
   * @param Limit Maximum number of results to return
   */
  def searchWords(Query: String, CaseSensitive: Option[String] = None, IncludePartOfSpeech: Option[String] = None, ExcludePartOfSpeech: Option[String] = None, MinCorpusCount: Option[Int] = None, MaxCorpusCount: Option[Int] = None, MinDictionaryCount: Option[Int] = None, MaxDictionaryCount: Option[Int] = None, MinLength: Option[Int] = None, MaxLength: Option[Int] = None, Skip: Option[Int] = None, Limit: Option[Int] = None): ApiRequest[WordSearchResults] =
    ApiRequest[WordSearchResults](ApiMethods.GET, "https://api.wordnik.com/v4", "/words.json/search/{query}", "application/json")
      .withQueryParam("caseSensitive", CaseSensitive)
      .withQueryParam("includePartOfSpeech", IncludePartOfSpeech)
      .withQueryParam("excludePartOfSpeech", ExcludePartOfSpeech)
      .withQueryParam("minCorpusCount", MinCorpusCount)
      .withQueryParam("maxCorpusCount", MaxCorpusCount)
      .withQueryParam("minDictionaryCount", MinDictionaryCount)
      .withQueryParam("maxDictionaryCount", MaxDictionaryCount)
      .withQueryParam("minLength", MinLength)
      .withQueryParam("maxLength", MaxLength)
      .withQueryParam("skip", Skip)
      .withQueryParam("limit", Limit)
      .withPathParam("query", Query)
      .withSuccessResponse[WordSearchResults](200)
      .withErrorResponse[Unit](400)
      
  /**
   * 
   * 
   * Expected answers:
   *   code 0 : WordOfTheDay (success)
   * 
   * @param Date Fetches by date in yyyy-MM-dd
   */
  def getWordOfTheDay(Date: Option[String] = None): ApiRequest[WordOfTheDay] =
    ApiRequest[WordOfTheDay](ApiMethods.GET, "https://api.wordnik.com/v4", "/words.json/wordOfTheDay", "application/json")
      .withQueryParam("date", Date)
      .withDefaultSuccessResponse[WordOfTheDay]
      


}

