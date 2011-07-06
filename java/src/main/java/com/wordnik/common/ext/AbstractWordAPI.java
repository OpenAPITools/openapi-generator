package com.wordnik.common.ext;

import com.wordnik.annotations.MethodArgumentNames;
import com.wordnik.common.WordnikAPI;
import com.wordnik.exception.WordnikAPIException;
import com.wordnik.exception.WordnikExceptionCodes;
import com.wordnik.model.AudioFile;
import com.wordnik.model.FrequencySummary;
import com.wordnik.model.GetFrequencyOutput;
import org.codehaus.jackson.map.type.TypeFactory;
import org.codehaus.jackson.type.TypeReference;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * This class is written to provide implementations for some of the methods that are not possible from code generation.
 * Example: For getting the word frequency we have the possiblity of to output bsed on the type of input, this will be
 * difficult to model with code generation hence the overwriten calls is provided and the ode generation ignores
 * these methods while generating the code.
 * User: ramesh
 * Date: 4/26/11
 * Time: 7:51 AM
 */
public abstract class AbstractWordAPI extends WordnikAPI {

    /**
     * Fetches audio metadata for a word.
     *
     * The metadata includes a time-expiring fileUrl which allows reading the audio file directly from the API.
     * Currently only audio pronunciations from the American Heritage Dictionary in mp3 format are supported.
     *
     * @param word  Word to get audio for.
     * @param useCanonical  Use the canonical form of the word.
     * @param limit  Maximum number of results to return
     *
     * @return GetAudioOutput {@link com.wordnik.model.AudioFile}
     * @throws com.wordnik.exception.WordnikAPIException 400 - Invalid word supplied. 400 - Invalid word supplied.
     */
     @MethodArgumentNames(value="word, useCanonical, limit")
    public static List<AudioFile> getAudio(String word, String useCanonical, String limit) throws WordnikAPIException {


        //parse inputs
        String  resourcePath = "/word.{format}/{word}/audio";
        resourcePath = resourcePath.replace("{format}","json");
        String method = "GET";
        Map<String, String> queryParams = new HashMap<String, String>();

        if( useCanonical != null) {
             queryParams.put("useCanonical", useCanonical);
        }
        if( limit != null) {
             queryParams.put("limit", limit);
        }

        if( word != null) {
            resourcePath = resourcePath.replace("{word}", word);
        }


        //make the API Call
        String response = invokeAPI(null, resourcePath, method, queryParams, null);
        //create output objects if the response has more than one object
        if(response == null || response.length() == 0){
            return null;
        }
        TypeReference ref = new TypeReference<List<AudioFile>>() { };
        try {
            List<AudioFile> responseObject = (List<AudioFile>)mapper.readValue(response, TypeFactory.type(ref));
            return responseObject;
        }catch(Exception e){
            throw new WordnikAPIException(WordnikExceptionCodes.ERROR_CONVERTING_JSON_TO_JAVA, e.getMessage());
            
        }
    }


    /**
     * Returns word usage over tim
     *
     * @param word  Word to return
     *
     * @param useCanonical  If true will try to return the correct word root ('cats' -> 'cat'). If false returns
     * exactly what was requested.
     *
     * @param startYear  Starting Year
     *
     * @param endYear  Ending Year
     *
     * @return GetFrequencyOutput {@link com.wordnik.model.GetFrequencyOutput}
     * @throws WordnikAPIException 400 - Invalid word supplied. 404 - No results. 400 - Invalid word supplied. 404 - No results.
     */
     @MethodArgumentNames(value="word, useCanonical, startYear, endYear")
    public static FrequencySummary getWordFrequency(String word, String useCanonical, String startYear,
                                                      String endYear) throws WordnikAPIException {


        //parse inputs
        String  resourcePath = "/word.{format}/{word}/frequency";
        resourcePath = resourcePath.replace("{format}","json");
        String method = "GET";
        Map<String, String> queryParams = new HashMap<String, String>();

        if( useCanonical != null) {
             queryParams.put("useCanonical", useCanonical);
        }
        if( startYear != null) {
             queryParams.put("startYear", startYear);
        }
        if( endYear != null) {
             queryParams.put("endYear", endYear);
        }

        if( word != null) {
            resourcePath = resourcePath.replace("{word}", word);
        }


        //make the API Call
        String response = invokeAPI(null, resourcePath, method, queryParams, null);
        //create output objects if the response has more than one object
        if(response == null || response.length() == 0){
            return null;
        }
        FrequencySummary responseObject = (FrequencySummary)deserialize(response, FrequencySummary.class);
        return responseObject;

    }

}
