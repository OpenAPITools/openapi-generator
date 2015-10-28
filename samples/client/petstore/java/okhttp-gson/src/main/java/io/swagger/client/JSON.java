package io.swagger.client;

import com.google.gson.Gson;
import com.google.gson.GsonBuilder;
import com.google.gson.JsonDeserializationContext;
import com.google.gson.JsonDeserializer;
import com.google.gson.JsonElement;
import com.google.gson.JsonNull;
import com.google.gson.JsonParseException;
import com.google.gson.JsonPrimitive;
import com.google.gson.JsonSerializationContext;
import com.google.gson.JsonSerializer;
import com.google.gson.stream.JsonReader;

import java.io.StringReader;
import java.lang.reflect.Type;
import java.util.Date;

public class JSON {
  private ApiClient apiClient;
  private Gson gson;

  public JSON(ApiClient apiClient) {
    this.apiClient = apiClient;
    gson = new GsonBuilder()
      .registerTypeAdapter(Date.class, new DateAdapter(apiClient))
      .create();
  }

  public Gson getGson() {
    return gson;
  }

  public void setGson(Gson gson) {
    this.gson = gson;
  }

  /**
   * Serialize the given Java object into JSON string.
   */
  public String serialize(Object obj) {
    return gson.toJson(obj);
  }

  /**
   * Deserialize the given JSON string to Java object.
   *
   * @param body The JSON string
   * @param returnType The type to deserialize inot
   * @return The deserialized Java object
   */
  public <T> T deserialize(String body, Type returnType) {
    try {
      if (apiClient.isLenientOnJson()) {
        JsonReader jsonReader = new JsonReader(new StringReader(body));
        // see https://google-gson.googlecode.com/svn/trunk/gson/docs/javadocs/com/google/gson/stream/JsonReader.html#setLenient(boolean)
        jsonReader.setLenient(true);
        return gson.fromJson(jsonReader, returnType);
      } else {
        return gson.fromJson(body, returnType);
      }
    } catch (JsonParseException e) {
      // Fallback processing when failed to parse JSON form response body:
      //   return the response body string directly for the String return type;
      //   parse response body into date or datetime for the Date return type.
      if (returnType.equals(String.class))
        return (T) body;
      else if (returnType.equals(Date.class))
        return (T) apiClient.parseDateOrDatetime(body);
      else throw(e);
    }
  }
}

class DateAdapter implements JsonSerializer<Date>, JsonDeserializer<Date> {
    private final ApiClient apiClient;

    public DateAdapter(ApiClient apiClient) {
        super();
        this.apiClient = apiClient;
    }

    @Override
    public JsonElement serialize(Date src, Type typeOfSrc, JsonSerializationContext context) {
        if (src == null) {
            return JsonNull.INSTANCE;
        } else {
            return new JsonPrimitive(apiClient.formatDatetime(src));
        }
    }

    @Override
    public Date deserialize(JsonElement json, Type date, JsonDeserializationContext context) throws JsonParseException {
        String str = json.getAsJsonPrimitive().getAsString();
        try {
            return apiClient.parseDateOrDatetime(str);
        } catch (RuntimeException e) {
            throw new JsonParseException(e);
        }
    }
}
