package helper;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.MapperFeature;
import com.fasterxml.jackson.databind.SerializationFeature;
import com.fasterxml.jackson.databind.util.StdDateFormat;
import com.fasterxml.jackson.datatype.jsr310.JavaTimeModule;
import org.approvaltests.Approvals;
import org.approvaltests.core.Options;
import org.springframework.web.reactive.function.client.WebClientRequestException;
import org.springframework.web.reactive.function.client.WebClientResponseException;

import java.text.SimpleDateFormat;

public class TestingHelper {

    private static Options removeTrailingSpaces = new Options(new StripTrailingWhitespaceScrubber());

    public static void approveException(WebClientResponseException exception) {
        String message = "Status code: " + exception.getStatusCode() +
                "\nReason: " + exception.getResponseBodyAsString() +
                "\nResponse headers: " + exception.getHeaders();
        Approvals.verify(message, removeTrailingSpaces);
    }

    public static void approveException(WebClientRequestException exception) {
        String message = "URI: " + exception.getUri() +
                "\nMethod: " + exception.getMethod () +
                "\nHeaders: " + exception.getHeaders() +
                "\nMessage: " + exception.getMessage();
        Approvals.verify(message, removeTrailingSpaces);
    }

    public static void approveResponseAsJson(Object responseObject) {
        ObjectMapper objectMapper = new ObjectMapper();
        objectMapper.registerModule(new JavaTimeModule());
        objectMapper.setDateFormat(new StdDateFormat());
        objectMapper.setConfig(objectMapper.getSerializationConfig()
           .with(MapperFeature.SORT_PROPERTIES_ALPHABETICALLY)
           .with(SerializationFeature.ORDER_MAP_ENTRIES_BY_KEYS));

        String json = "";

        try {
            json = objectMapper.writerWithDefaultPrettyPrinter().writeValueAsString(responseObject);
        } catch (JsonProcessingException e) {
            json = e.toString();
        }
        Approvals.verify(json, removeTrailingSpaces);
    }


    public static long errorRaisingStringToInt(String errorRisingString) {
        if (errorRisingString.equals("ThrowsApiException")) {
            return 9100;
        }
        else if (errorRisingString.equals("ThrowsStdExceptionDerivedException")) {
            return 9200;
        }
        else if (errorRisingString.equals("ThrowsInt")) {
            return 9300;
        }

        String status = errorRisingString.replace("ReturnsStatus", "");

        return Integer.parseInt(status);
    }
}
