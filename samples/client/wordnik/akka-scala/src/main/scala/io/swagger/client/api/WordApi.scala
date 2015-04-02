package io.swagger.client.api

import io.swagger.client.model.WordObject
import io.swagger.client.model.AudioFile
import io.swagger.client.model.Definition
import io.swagger.client.model.FrequencySummary
import io.swagger.client.model.Bigram
import io.swagger.client.model.Example
import io.swagger.client.core._
import io.swagger.client.core.CollectionFormats._
import io.swagger.client.core.ApiKeyLocations._

object WordApi {

  /**
   * 
   * 
   * Expected answers:
   *   code 200 : WordObject (success)
   *   code 400 :  (Invalid word supplied.)
   * 
   * @param Word String value of WordObject to return
   * @param UseCanonical If true will try to return the correct word root (&#39;cats&#39; -&gt; &#39;cat&#39;). If false returns exactly what was requested.
   * @param IncludeSuggestions Return suggestions (for correct spelling, case variants, etc.)
   */
  def getWord(Word: String, UseCanonical: Option[String] = None, IncludeSuggestions: Option[String] = None): ApiRequest[WordObject] =
    ApiRequest[WordObject](ApiMethods.GET, "https://api.wordnik.com/v4", "/word.json/{word}", "application/json")
      .withQueryParam("useCanonical", UseCanonical)
      .withQueryParam("includeSuggestions", IncludeSuggestions)
      .withPathParam("word", Word)
      .withSuccessResponse[WordObject](200)
      .withErrorResponse[Unit](400)
      
  /**
   * The metadata includes a time-expiring fileUrl which allows reading the audio file directly from the API.  Currently only audio pronunciations from the American Heritage Dictionary in mp3 format are supported.
   * 
   * Expected answers:
   *   code 200 : Seq[AudioFile] (success)
   *   code 400 :  (Invalid word supplied.)
   * 
   * @param Word Word to get audio for.
   * @param UseCanonical Use the canonical form of the word
   * @param Limit Maximum number of results to return
   */
  def getAudio(Word: String, UseCanonical: Option[String] = None, Limit: Option[Int] = None): ApiRequest[Seq[AudioFile]] =
    ApiRequest[Seq[AudioFile]](ApiMethods.GET, "https://api.wordnik.com/v4", "/word.json/{word}/audio", "application/json")
      .withQueryParam("useCanonical", UseCanonical)
      .withQueryParam("limit", Limit)
      .withPathParam("word", Word)
      .withSuccessResponse[Seq[AudioFile]](200)
      .withErrorResponse[Unit](400)
      
  /**
   * 
   * 
   * Expected answers:
   *   code 200 : Seq[Definition] (success)
   *   code 400 :  (Invalid word supplied.)
   *   code 404 :  (No definitions found.)
   * 
   * @param Word Word to return definitions for
   * @param Limit Maximum number of results to return
   * @param PartOfSpeech CSV list of part-of-speech types
   * @param IncludeRelated Return related words with definitions
   * @param SourceDictionaries Source dictionary to return definitions from.  If &#39;all&#39; is received, results are returned from all sources. If multiple values are received (e.g. &#39;century,wiktionary&#39;), results are returned from the first specified dictionary that has definitions. If left blank, results are returned from the first dictionary that has definitions. By default, dictionaries are searched in this order: ahd, wiktionary, webster, century, wordnet
   * @param UseCanonical If true will try to return the correct word root (&#39;cats&#39; -&gt; &#39;cat&#39;). If false returns exactly what was requested.
   * @param IncludeTags Return a closed set of XML tags in response
   */
  def getDefinitions(Word: String, Limit: Option[Int] = None, PartOfSpeech: Option[String] = None, IncludeRelated: Option[String] = None, SourceDictionaries: Seq[String], UseCanonical: Option[String] = None, IncludeTags: Option[String] = None): ApiRequest[Seq[Definition]] =
    ApiRequest[Seq[Definition]](ApiMethods.GET, "https://api.wordnik.com/v4", "/word.json/{word}/definitions", "application/json")
      .withQueryParam("limit", Limit)
      .withQueryParam("partOfSpeech", PartOfSpeech)
      .withQueryParam("includeRelated", IncludeRelated)
      .withQueryParam("sourceDictionaries", ArrayValues(SourceDictionaries, CSV))
      .withQueryParam("useCanonical", UseCanonical)
      .withQueryParam("includeTags", IncludeTags)
      .withPathParam("word", Word)
      .withSuccessResponse[Seq[Definition]](200)
      .withErrorResponse[Unit](400)
      .withErrorResponse[Unit](404)
      
  /**
   * 
   * 
   * Expected answers:
   *   code 200 : Seq[String] (success)
   *   code 400 :  (Invalid word supplied.)
   *   code 404 :  (No definitions found.)
   * 
   * @param Word Word to return
   * @param UseCanonical If true will try to return the correct word root (&#39;cats&#39; -&gt; &#39;cat&#39;). If false returns exactly what was requested.
   */
  def getEtymologies(Word: String, UseCanonical: Option[String] = None): ApiRequest[Seq[String]] =
    ApiRequest[Seq[String]](ApiMethods.GET, "https://api.wordnik.com/v4", "/word.json/{word}/etymologies", "application/json")
      .withQueryParam("useCanonical", UseCanonical)
      .withPathParam("word", Word)
      .withSuccessResponse[Seq[String]](200)
      .withErrorResponse[Unit](400)
      .withErrorResponse[Unit](404)
      
  /**
   * 
   * 
   * Expected answers:
   *   code 200 :  (success)
   *   code 400 :  (Invalid word supplied.)
   * 
   * @param Word Word to return examples for
   * @param IncludeDuplicates Show duplicate examples from different sources
   * @param UseCanonical If true will try to return the correct word root (&#39;cats&#39; -&gt; &#39;cat&#39;). If false returns exactly what was requested.
   * @param Skip Results to skip
   * @param Limit Maximum number of results to return
   */
  def getExamples(Word: String, IncludeDuplicates: Option[String] = None, UseCanonical: Option[String] = None, Skip: Option[Int] = None, Limit: Option[Int] = None): ApiRequest[Unit] =
    ApiRequest[Unit](ApiMethods.GET, "https://api.wordnik.com/v4", "/word.json/{word}/examples", "application/json")
      .withQueryParam("includeDuplicates", IncludeDuplicates)
      .withQueryParam("useCanonical", UseCanonical)
      .withQueryParam("skip", Skip)
      .withQueryParam("limit", Limit)
      .withPathParam("word", Word)
      .withSuccessResponse[Unit](200)
      .withErrorResponse[Unit](400)
      
  /**
   * 
   * 
   * Expected answers:
   *   code 200 : FrequencySummary (success)
   *   code 400 :  (Invalid word supplied.)
   *   code 404 :  (No results.)
   * 
   * @param Word Word to return
   * @param UseCanonical If true will try to return the correct word root (&#39;cats&#39; -&gt; &#39;cat&#39;). If false returns exactly what was requested.
   * @param StartYear Starting Year
   * @param EndYear Ending Year
   */
  def getWordFrequency(Word: String, UseCanonical: Option[String] = None, StartYear: Option[Int] = None, EndYear: Option[Int] = None): ApiRequest[FrequencySummary] =
    ApiRequest[FrequencySummary](ApiMethods.GET, "https://api.wordnik.com/v4", "/word.json/{word}/frequency", "application/json")
      .withQueryParam("useCanonical", UseCanonical)
      .withQueryParam("startYear", StartYear)
      .withQueryParam("endYear", EndYear)
      .withPathParam("word", Word)
      .withSuccessResponse[FrequencySummary](200)
      .withErrorResponse[Unit](400)
      .withErrorResponse[Unit](404)
      
  /**
   * 
   * 
   * Expected answers:
   *   code 200 :  (success)
   *   code 400 :  (Invalid word supplied.)
   * 
   * @param Word Word to get syllables for
   * @param UseCanonical If true will try to return a correct word root (&#39;cats&#39; -&gt; &#39;cat&#39;). If false returns exactly what was requested.
   * @param SourceDictionary Get from a single dictionary. Valid options: ahd, century, wiktionary, webster, and wordnet.
   * @param Limit Maximum number of results to return
   */
  def getHyphenation(Word: String, UseCanonical: Option[String] = None, SourceDictionary: Option[String] = None, Limit: Option[Int] = None): ApiRequest[Unit] =
    ApiRequest[Unit](ApiMethods.GET, "https://api.wordnik.com/v4", "/word.json/{word}/hyphenation", "application/json")
      .withQueryParam("useCanonical", UseCanonical)
      .withQueryParam("sourceDictionary", SourceDictionary)
      .withQueryParam("limit", Limit)
      .withPathParam("word", Word)
      .withSuccessResponse[Unit](200)
      .withErrorResponse[Unit](400)
      
  /**
   * 
   * 
   * Expected answers:
   *   code 200 : Seq[Bigram] (success)
   *   code 400 :  (Invalid word supplied.)
   * 
   * @param Word Word to fetch phrases for
   * @param Limit Maximum number of results to return
   * @param Wlmi Minimum WLMI for the phrase
   * @param UseCanonical If true will try to return the correct word root (&#39;cats&#39; -&gt; &#39;cat&#39;). If false returns exactly what was requested.
   */
  def getPhrases(Word: String, Limit: Option[Int] = None, Wlmi: Option[Int] = None, UseCanonical: Option[String] = None): ApiRequest[Seq[Bigram]] =
    ApiRequest[Seq[Bigram]](ApiMethods.GET, "https://api.wordnik.com/v4", "/word.json/{word}/phrases", "application/json")
      .withQueryParam("limit", Limit)
      .withQueryParam("wlmi", Wlmi)
      .withQueryParam("useCanonical", UseCanonical)
      .withPathParam("word", Word)
      .withSuccessResponse[Seq[Bigram]](200)
      .withErrorResponse[Unit](400)
      
  /**
   * 
   * 
   * Expected answers:
   *   code 200 :  (success)
   *   code 400 :  (Invalid word supplied.)
   * 
   * @param Word Word to get pronunciations for
   * @param UseCanonical If true will try to return a correct word root (&#39;cats&#39; -&gt; &#39;cat&#39;). If false returns exactly what was requested.
   * @param SourceDictionary Get from a single dictionary
   * @param TypeFormat Text pronunciation type
   * @param Limit Maximum number of results to return
   */
  def getTextPronunciations(Word: String, UseCanonical: Option[String] = None, SourceDictionary: Option[String] = None, TypeFormat: Option[String] = None, Limit: Option[Int] = None): ApiRequest[Unit] =
    ApiRequest[Unit](ApiMethods.GET, "https://api.wordnik.com/v4", "/word.json/{word}/pronunciations", "application/json")
      .withQueryParam("useCanonical", UseCanonical)
      .withQueryParam("sourceDictionary", SourceDictionary)
      .withQueryParam("typeFormat", TypeFormat)
      .withQueryParam("limit", Limit)
      .withPathParam("word", Word)
      .withSuccessResponse[Unit](200)
      .withErrorResponse[Unit](400)
      
  /**
   * 
   * 
   * Expected answers:
   *   code 200 :  (success)
   *   code 400 :  (Invalid word supplied.)
   * 
   * @param Word Word to fetch relationships for
   * @param UseCanonical If true will try to return the correct word root (&#39;cats&#39; -&gt; &#39;cat&#39;). If false returns exactly what was requested.
   * @param RelationshipTypes Limits the total results per type of relationship type
   * @param LimitPerRelationshipType Restrict to the supplied relationship types
   */
  def getRelatedWords(Word: String, UseCanonical: Option[String] = None, RelationshipTypes: Option[String] = None, LimitPerRelationshipType: Option[Int] = None): ApiRequest[Unit] =
    ApiRequest[Unit](ApiMethods.GET, "https://api.wordnik.com/v4", "/word.json/{word}/relatedWords", "application/json")
      .withQueryParam("useCanonical", UseCanonical)
      .withQueryParam("relationshipTypes", RelationshipTypes)
      .withQueryParam("limitPerRelationshipType", LimitPerRelationshipType)
      .withPathParam("word", Word)
      .withSuccessResponse[Unit](200)
      .withErrorResponse[Unit](400)
      
  /**
   * 
   * 
   * Expected answers:
   *   code 200 : Example (success)
   *   code 400 :  (Invalid word supplied.)
   * 
   * @param Word Word to fetch examples for
   * @param UseCanonical If true will try to return the correct word root (&#39;cats&#39; -&gt; &#39;cat&#39;). If false returns exactly what was requested.
   */
  def getTopExample(Word: String, UseCanonical: Option[String] = None): ApiRequest[Example] =
    ApiRequest[Example](ApiMethods.GET, "https://api.wordnik.com/v4", "/word.json/{word}/topExample", "application/json")
      .withQueryParam("useCanonical", UseCanonical)
      .withPathParam("word", Word)
      .withSuccessResponse[Example](200)
      .withErrorResponse[Unit](400)
      


}

