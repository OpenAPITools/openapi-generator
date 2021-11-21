package org.openapitools.codegen.serializer;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.MapperFeature;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.module.SimpleModule;
import com.fasterxml.jackson.dataformat.yaml.YAMLFactory;
import com.fasterxml.jackson.dataformat.yaml.YAMLGenerator;
import io.swagger.v3.core.util.Json;
import io.swagger.v3.core.util.Yaml;
import io.swagger.v3.oas.models.OpenAPI;
import org.apache.commons.lang3.time.DateUtils;
import org.openapitools.codegen.config.GlobalSettings;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.text.ParseException;

public class SerializerUtils {
    private static final Logger LOGGER = LoggerFactory.getLogger(SerializerUtils.class);
    private static final String YAML_MINIMIZE_QUOTES_PROPERTY = "org.openapitools.codegen.utils.yaml.minimize.quotes";
    private static final boolean minimizeYamlQuotes = Boolean.parseBoolean(GlobalSettings.getProperty(YAML_MINIMIZE_QUOTES_PROPERTY, "true"));
    // Checked all the types included in the date.
    private static final String[] parsePatterns = {"yyyy-MM-dd HH:mm:ss","yyyy-MM-dd HH:mm","yyyy-MM-dd", "yyyy/MM/dd HH:mm:ss","yyyy/MM/dd HH:mm","yyyy/MM/dd HH:mm","yyyy/MM/dd"};
    private static final String string1 = "String";
    private static final String string2 = "string";


    public static String toYamlString(OpenAPI openAPI) {
        if (openAPI == null) {
            return null;
        }
        SimpleModule module = createModule();

        try {
            ObjectMapper yamlMapper = Yaml.mapper().copy();
            // there is an unfortunate YAML condition where user inputs should be treated as strings (e.g. "1234_1234"), but in yaml this is a valid number and
            // removing quotes forcibly by default means we are potentially doing a data conversion resulting in an unexpected change to the user's YAML outputs.
            // We may allow for property-based enable/disable, retaining the default of enabled for backward compatibility.

            // When type is string, then use the user input time.
            if(openAPI.toString().contains("type: string") && checkValidTime(openAPI.toString())) {
                ((YAMLFactory) yamlMapper.getFactory()).disable(YAMLGenerator.Feature.MINIMIZE_QUOTES);
            }else if (minimizeYamlQuotes) {
                ((YAMLFactory) yamlMapper.getFactory()).enable(YAMLGenerator.Feature.MINIMIZE_QUOTES);
            } else {
                ((YAMLFactory) yamlMapper.getFactory()).disable(YAMLGenerator.Feature.MINIMIZE_QUOTES);
            }
            return yamlMapper.registerModule(module)
                    .configure(MapperFeature.SORT_PROPERTIES_ALPHABETICALLY, true)
                    .writeValueAsString(openAPI)
                    .replace("\r\n", "\n");

        } catch (JsonProcessingException e) {
            LOGGER.warn("Can not create yaml content", e);

        }
        return null;
    }

    /**
     * Calculate how many spaces in one input line before the first character.
     */
    private static int countFrontSpace(String s){
        int count = 0;
        for(char c : s.toCharArray()){
            if(c == ' '){
                count++;
            } else{
                return count;
            }
        }
        return (int)Integer.MAX_VALUE;
    }

    /**
     * Check yaml contains data type value and also contains type:String
     */
    private static boolean checkValidTime(String string){
        String[] strings = string.split("\n");
        boolean typeString ;
        boolean dateForm ;

        for(int i = 0; i < strings.length; i++){
            String st = strings[i];
            if(st.contains("Schema") || st.contains("schema") ){
                int space = countFrontSpace(st);
                dateForm = false;
                typeString = false;
                String st2 = strings[++i];
                while( countFrontSpace(st2) > space) {
                    st2 = strings[i];  i++;
                    dateForm = dateForm || checkDateForm(st2);
                    typeString = typeString || checkTypeString(st2);
                    if (typeString && dateForm) {
                        return true;
                    }
                }
            }
        }
        return false;
    }

    /**
     * Check String include date time value
     */
    private static boolean checkDateForm(String st){
        String[] lines = st.split(":",2);

        if(lines.length < 2 || lines[1] == null || lines[1].length() == 0){
            return false;
        }
        try{
            DateUtils.parseDate(lines[1].trim(),parsePatterns);
            return true;
        }catch(ParseException e){
            return false;
        }
    }

    /**
     * Check type as "String" or "string"
     */
    private static boolean checkTypeString(String st){
        String[] lines = st.split(":");

        if(lines.length < 2 || lines[1] == null || lines[1].length() == 0){
            return false;
        }else{
            String re = lines[1].trim();
            return re != null && (re.equals(string1) || re.equals(string2));
        }
    }


    public static String toJsonString(OpenAPI openAPI) {
        if (openAPI == null) {
            return null;
        }

        SimpleModule module = createModule();
        try {
            return Json.mapper()
                    .copy()
                    .registerModule(module)
                    .configure(MapperFeature.SORT_PROPERTIES_ALPHABETICALLY, true)
                    .writerWithDefaultPrettyPrinter()
                    .writeValueAsString(openAPI)
                    .replace("\r\n", "\n");
        } catch (JsonProcessingException e) {
            LOGGER.warn("Can not create json content", e);
        }
        return null;
    }

    private static SimpleModule createModule() {
        SimpleModule module = new SimpleModule("OpenAPIModule");
        module.addSerializer(OpenAPI.class, new OpenAPISerializer());
        return module;
    }
}
