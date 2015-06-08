package io.swagger.client.api

object WordApi {

  /**
   *
   *
   * Expected answers:
   * code 200 : WordObject (success)
   * code 400 :  (Invalid word supplied.)
   *
   * @param word String value of WordObject to return
   * @param useCanonical If true will try to return the correct word root (&#39;cats&#39; -&gt; &#39;cat&#39;). If false returns exactly what was requested.
   * @param includeSuggestions Return suggestions (for correct spelling, case variants, etc.)
   */
  def getWord(word: String, useCanonical: Option[String] = None, includeSuggestions: Option[String] = None): ApiRequest[WordObject] =
    ApiRequest[WordObject](ApiMethods.GET, "https://api.wordnik.com/v4", "/word.json/{word}", "application/json")
      .withQueryParam("useCanonical", useCanonical)
      .withQueryParam("includeSuggestions", includeSuggestions)
      .withPathParam("word", word)
      .withSuccessResponse[WordObject](200)
      .withErrorResponse[Unit](400)

  /**
   * The metadata includes a time-expiring fileUrl which allows reading the audio file directly from the API.  Currently only audio pronunciations from the American Heritage Dictionary in mp3 format are supported.
   *
   * Expected answers:
   * code 200 : Seq[AudioFile] (success)
   * code 400 :  (Invalid word supplied.)
   *
   * @param word Word to get audio for.
   * @param useCanonical Use the canonical form of the word
   * @param limit Maximum number of results to return
   */
  def getAudio(word: String, useCanonical: Option[String] = None, limit: Option[Int] = None): ApiRequest[Seq[AudioFile]] =
    ApiRequest[Seq[AudioFile]](ApiMethods.GET, "https://api.wordnik.com/v4", "/word.json/{word}/audio", "application/json")
      .withQueryParam("useCanonical", useCanonical)
      .withQueryParam("limit", limit)
      .withPathParam("word", word)
      .withSuccessResponse[Seq[AudioFile]](200)
      .withErrorResponse[Unit](400)

  /**
   *
   *
   * Expected answers:
   * code 200 : Seq[Definition] (success)
   * code 400 :  (Invalid word supplied.)
   * code 404 :  (No definitions found.)
   *
   * @param word Word to return definitions for
   * @param limit Maximum number of results to return
   * @param partOfSpeech CSV list of part-of-speech types
   * @param includeRelated Return related words with definitions
   * @param sourceDictionaries Source dictionary to return definitions from.  If &#39;all&#39; is received, results are returned from all sources. If multiple values are received (e.g. &#39;century,wiktionary&#39;), results are returned from the first specified dictionary that has definitions. If left blank, results are returned from the first dictionary that has definitions. By default, dictionaries are searched in this order: ahd, wiktionary, webster, century, wordnet
   * @param useCanonical If true will try to return the correct word root (&#39;cats&#39; -&gt; &#39;cat&#39;). If false returns exactly what was requested.
   * @param includeTags Return a closed set of XML tags in response
   */
  def getDefinitions(word: String, limit: Option[Int] = None, partOfSpeech: Option[String] = None, includeRelated: Option[String] = None, sourceDictionaries: Seq[String], useCanonical: Option[String] = None, includeTags: Option[String] = None): ApiRequest[Seq[Definition]] =
    ApiRequest[Seq[Definition]](ApiMethods.GET, "https://api.wordnik.com/v4", "/word.json/{word}/definitions", "application/json")
      .withQueryParam("limit", limit)
      .withQueryParam("partOfSpeech", partOfSpeech)
      .withQueryParam("includeRelated", includeRelated)
      .withQueryParam("sourceDictionaries", ArrayValues(sourceDictionaries, CSV))
      .withQueryParam("useCanonical", useCanonical)
      .withQueryParam("includeTags", includeTags)
      .withPathParam("word", word)
      .withSuccessResponse[Seq[Definition]](200)
      .withErrorResponse[Unit](400)
      .withErrorResponse[Unit](404)

  /**
   *
   *
   * Expected answers:
   * code 200 : Seq[String] (success)
   * code 400 :  (Invalid word supplied.)
   * code 404 :  (No definitions found.)
   *
   * @param word Word to return
   * @param useCanonical If true will try to return the correct word root (&#39;cats&#39; -&gt; &#39;cat&#39;). If false returns exactly what was requested.
   */
  def getEtymologies(word: String, useCanonical: Option[String] = None): ApiRequest[Seq[String]] =
    ApiRequest[Seq[String]](ApiMethods.GET, "https://api.wordnik.com/v4", "/word.json/{word}/etymologies", "application/json")
      .withQueryParam("useCanonical", useCanonical)
      .withPathParam("word", word)
      .withSuccessResponse[Seq[String]](200)
      .withErrorResponse[Unit](400)
      .withErrorResponse[Unit](404)

  /**
   *
   *
   * Expected answers:
   * code 200 :  (success)
   * code 400 :  (Invalid word supplied.)
   *
   * @param word Word to return examples for
   * @param includeDuplicates Show duplicate examples from different sources
   * @param useCanonical If true will try to return the correct word root (&#39;cats&#39; -&gt; &#39;cat&#39;). If false returns exactly what was requested.
   * @param skip Results to skip
   * @param limit Maximum number of results to return
   */
  def getExamples(word: String, includeDuplicates: Option[String] = None, useCanonical: Option[String] = None, skip: Option[Int] = None, limit: Option[Int] = None): ApiRequest[Unit] =
    ApiRequest[Unit](ApiMethods.GET, "https://api.wordnik.com/v4", "/word.json/{word}/examples", "application/json")
      .withQueryParam("includeDuplicates", includeDuplicates)
      .withQueryParam("useCanonical", useCanonical)
      .withQueryParam("skip", skip)
      .withQueryParam("limit", limit)
      .withPathParam("word", word)
      .withSuccessResponse[Unit](200)
      .withErrorResponse[Unit](400)

  /**
   *
   *
   * Expected answers:
   * code 200 : FrequencySummary (success)
   * code 400 :  (Invalid word supplied.)
   * code 404 :  (No results.)
   *
   * @param word Word to return
   * @param useCanonical If true will try to return the correct word root (&#39;cats&#39; -&gt; &#39;cat&#39;). If false returns exactly what was requested.
   * @param startYear Starting Year
   * @param endYear Ending Year
   */
  def getWordFrequency(word: String, useCanonical: Option[String] = None, startYear: Option[Int] = None, endYear: Option[Int] = None): ApiRequest[FrequencySummary] =
    ApiRequest[FrequencySummary](ApiMethods.GET, "https://api.wordnik.com/v4", "/word.json/{word}/frequency", "application/json")
      .withQueryParam("useCanonical", useCanonical)
      .withQueryParam("startYear", startYear)
      .withQueryParam("endYear", endYear)
      .withPathParam("word", word)
      .withSuccessResponse[FrequencySummary](200)
      .withErrorResponse[Unit](400)
      .withErrorResponse[Unit](404)

  /**
   *
   *
   * Expected answers:
   * code 200 :  (success)
   * code 400 :  (Invalid word supplied.)
   *
   * @param word Word to get syllables for
   * @param useCanonical If true will try to return a correct word root (&#39;cats&#39; -&gt; &#39;cat&#39;). If false returns exactly what was requested.
   * @param sourceDictionary Get from a single dictionary. Valid options: ahd, century, wiktionary, webster, and wordnet.
   * @param limit Maximum number of results to return
   */
  def getHyphenation(word: String, useCanonical: Option[String] = None, sourceDictionary: Option[String] = None, limit: Option[Int] = None): ApiRequest[Unit] =
    ApiRequest[Unit](ApiMethods.GET, "https://api.wordnik.com/v4", "/word.json/{word}/hyphenation", "application/json")
      .withQueryParam("useCanonical", useCanonical)
      .withQueryParam("sourceDictionary", sourceDictionary)
      .withQueryParam("limit", limit)
      .withPathParam("word", word)
      .withSuccessResponse[Unit](200)
      .withErrorResponse[Unit](400)

  /**
   *
   *
   * Expected answers:
   * code 200 : Seq[Bigram] (success)
   * code 400 :  (Invalid word supplied.)
   *
   * @param word Word to fetch phrases for
   * @param limit Maximum number of results to return
   * @param wlmi Minimum WLMI for the phrase
   * @param useCanonical If true will try to return the correct word root (&#39;cats&#39; -&gt; &#39;cat&#39;). If false returns exactly what was requested.
   */
  def getPhrases(word: String, limit: Option[Int] = None, wlmi: Option[Int] = None, useCanonical: Option[String] = None): ApiRequest[Seq[Bigram]] =
    ApiRequest[Seq[Bigram]](ApiMethods.GET, "https://api.wordnik.com/v4", "/word.json/{word}/phrases", "application/json")
      .withQueryParam("limit", limit)
      .withQueryParam("wlmi", wlmi)
      .withQueryParam("useCanonical", useCanonical)
      .withPathParam("word", word)
      .withSuccessResponse[Seq[Bigram]](200)
      .withErrorResponse[Unit](400)

  /**
   *
   *
   * Expected answers:
   * code 200 :  (success)
   * code 400 :  (Invalid word supplied.)
   *
   * @param word Word to get pronunciations for
   * @param useCanonical If true will try to return a correct word root (&#39;cats&#39; -&gt; &#39;cat&#39;). If false returns exactly what was requested.
   * @param sourceDictionary Get from a single dictionary
   * @param typeFormat Text pronunciation type
   * @param limit Maximum number of results to return
   */
  def getTextPronunciations(word: String, useCanonical: Option[String] = None, sourceDictionary: Option[String] = None, typeFormat: Option[String] = None, limit: Option[Int] = None): ApiRequest[Unit] =
    ApiRequest[Unit](ApiMethods.GET, "https://api.wordnik.com/v4", "/word.json/{word}/pronunciations", "application/json")
      .withQueryParam("useCanonical", useCanonical)
      .withQueryParam("sourceDictionary", sourceDictionary)
      .withQueryParam("typeFormat", typeFormat)
      .withQueryParam("limit", limit)
      .withPathParam("word", word)
      .withSuccessResponse[Unit](200)
      .withErrorResponse[Unit](400)

  /**
   *
   *
   * Expected answers:
   * code 200 :  (success)
   * code 400 :  (Invalid word supplied.)
   *
   * @param word Word to fetch relationships for
   * @param useCanonical If true will try to return the correct word root (&#39;cats&#39; -&gt; &#39;cat&#39;). If false returns exactly what was requested.
   * @param relationshipTypes Limits the total results per type of relationship type
   * @param limitPerRelationshipType Restrict to the supplied relationship types
   */
  def getRelatedWords(word: String, useCanonical: Option[String] = None, relationshipTypes: Option[String] = None, limitPerRelationshipType: Option[Int] = None): ApiRequest[Unit] =
    ApiRequest[Unit](ApiMethods.GET, "https://api.wordnik.com/v4", "/word.json/{word}/relatedWords", "application/json")
      .withQueryParam("useCanonical", useCanonical)
      .withQueryParam("relationshipTypes", relationshipTypes)
      .withQueryParam("limitPerRelationshipType", limitPerRelationshipType)
      .withPathParam("word", word)
      .withSuccessResponse[Unit](200)
      .withErrorResponse[Unit](400)

  /**
   *
   *
   * Expected answers:
   * code 200 : Example (success)
   * code 400 :  (Invalid word supplied.)
   *
   * @param word Word to fetch examples for
   * @param useCanonical If true will try to return the correct word root (&#39;cats&#39; -&gt; &#39;cat&#39;). If false returns exactly what was requested.
   */
  def getTopExample(word: String, useCanonical: Option[String] = None): ApiRequest[Example] =
    ApiRequest[Example](ApiMethods.GET, "https://api.wordnik.com/v4", "/word.json/{word}/topExample", "application/json")
      .withQueryParam("useCanonical", useCanonical)
      .withPathParam("word", word)
      .withSuccessResponse[Example](200)
      .withErrorResponse[Unit](400)


}

