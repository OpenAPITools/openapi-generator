import com.google.gson.Gson;
import com.google.gson.GsonBuilder;
import com.google.gson.JsonArray;
import com.google.gson.JsonDeserializationContext;
import com.google.gson.JsonDeserializer;
import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import com.google.gson.JsonParseException;
import com.google.gson.TypeAdapterFactory;
import com.google.gson.reflect.TypeToken;
import com.google.gson.TypeAdapter;
import com.google.gson.stream.JsonReader;
import com.google.gson.stream.JsonWriter;
import com.google.gson.annotations.JsonAdapter;
import com.google.gson.annotations.SerializedName;
import java.io.IOException;
import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.openapitools.client.JSON;

/**
 * TestQueryStyleDeepObjectExplodeTrueObjectAllOfQueryObjectParameter
 */
@javax.annotation.Generated(value = "org.openapitools.codegen.languages.JavaClientCodegen", comments = "Generator version: 7.26.0-SNAPSHOT")
public class TestQueryStyleDeepObjectExplodeTrueObjectAllOfQueryObjectParameter {
  public TestQueryStyleDeepObjectExplodeTrueObjectAllOfQueryObjectParameter() {
  }


  @Override
  public boolean equals(Object o) {
    if (this == o) {
      return true;
    }
    if (o == null || getClass() != o.getClass()) {
      return false;
    }
    return true;
  }

  @Override
  public int hashCode() {
    return Objects.hash();
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("class TestQueryStyleDeepObjectExplodeTrueObjectAllOfQueryObjectParameter {\n");
    sb.append("}");
    return sb.toString();
  }

  /**
   * Convert the given object to string with each line indented by 4 spaces
   * (except the first line).
   */
  private String toIndentedString(Object o) {
    return o == null ? "null" : o.toString().replace("\n", "\n    ");
  }


  public static HashSet<String> openapiFields;
  public static HashSet<String> openapiRequiredFields;

  static {
    // a set of all properties/fields (JSON key names)
    openapiFields = new HashSet<String>(0);

    // a set of required properties/fields (JSON key names)
    openapiRequiredFields = new HashSet<String>(0);
  }

  /**
   * Validates the JSON Element and throws an exception if issues found
   *
   * @param jsonElement JSON Element
   * @throws IOException if the JSON Element is invalid with respect to TestQueryStyleDeepObjectExplodeTrueObjectAllOfQueryObjectParameter
   */
  public static void validateJsonElement(JsonElement jsonElement) throws IOException {
      if (jsonElement == null) {
        if (!TestQueryStyleDeepObjectExplodeTrueObjectAllOfQueryObjectParameter.openapiRequiredFields.isEmpty()) { // has required fields but JSON element is null
          throw new IllegalArgumentException(String.format(java.util.Locale.ROOT, "The required field(s) %s in TestQueryStyleDeepObjectExplodeTrueObjectAllOfQueryObjectParameter is not found in the empty JSON string", TestQueryStyleDeepObjectExplodeTrueObjectAllOfQueryObjectParameter.openapiRequiredFields.toString()));
        }
      }

      Set<Map.Entry<String, JsonElement>> entries = jsonElement.getAsJsonObject().entrySet();
      // check to see if the JSON string contains additional fields
      for (Map.Entry<String, JsonElement> entry : entries) {
        if (!TestQueryStyleDeepObjectExplodeTrueObjectAllOfQueryObjectParameter.openapiFields.contains(entry.getKey())) {
          throw new IllegalArgumentException(String.format(java.util.Locale.ROOT, "The field `%s` in the JSON string is not defined in the `TestQueryStyleDeepObjectExplodeTrueObjectAllOfQueryObjectParameter` properties. JSON: %s", entry.getKey(), jsonElement.toString()));
        }
      }
  }

  public static class CustomTypeAdapterFactory implements TypeAdapterFactory {
    @SuppressWarnings("unchecked")
    @Override
    public <T> TypeAdapter<T> create(Gson gson, TypeToken<T> type) {
       if (!TestQueryStyleDeepObjectExplodeTrueObjectAllOfQueryObjectParameter.class.isAssignableFrom(type.getRawType())) {
         return null; // this class only serializes 'TestQueryStyleDeepObjectExplodeTrueObjectAllOfQueryObjectParameter' and its subtypes
       }
       final TypeAdapter<JsonElement> elementAdapter = gson.getAdapter(JsonElement.class);
       final TypeAdapter<TestQueryStyleDeepObjectExplodeTrueObjectAllOfQueryObjectParameter> thisAdapter
                        = gson.getDelegateAdapter(this, TypeToken.get(TestQueryStyleDeepObjectExplodeTrueObjectAllOfQueryObjectParameter.class));

       return (TypeAdapter<T>) new TypeAdapter<TestQueryStyleDeepObjectExplodeTrueObjectAllOfQueryObjectParameter>() {
           @Override
           public void write(JsonWriter out, TestQueryStyleDeepObjectExplodeTrueObjectAllOfQueryObjectParameter value) throws IOException {
             JsonObject obj = thisAdapter.toJsonTree(value).getAsJsonObject();
             elementAdapter.write(out, obj);
           }

           @Override
           public TestQueryStyleDeepObjectExplodeTrueObjectAllOfQueryObjectParameter read(JsonReader in) throws IOException {
             JsonElement jsonElement = elementAdapter.read(in);
             validateJsonElement(jsonElement);
             return thisAdapter.fromJsonTree(jsonElement);
           }

       }.nullSafe();
    }
  }

  /**
   * Create an instance of TestQueryStyleDeepObjectExplodeTrueObjectAllOfQueryObjectParameter given an JSON string
   *
   * @param jsonString JSON string
   * @return An instance of TestQueryStyleDeepObjectExplodeTrueObjectAllOfQueryObjectParameter
   * @throws IOException if the JSON string is invalid with respect to TestQueryStyleDeepObjectExplodeTrueObjectAllOfQueryObjectParameter
   */
  public static TestQueryStyleDeepObjectExplodeTrueObjectAllOfQueryObjectParameter fromJson(String jsonString) throws IOException {
    return JSON.getGson().fromJson(jsonString, TestQueryStyleDeepObjectExplodeTrueObjectAllOfQueryObjectParameter.class);
  }

  /**
   * Convert an instance of TestQueryStyleDeepObjectExplodeTrueObjectAllOfQueryObjectParameter to an JSON string
   *
   * @return JSON string
   */
  public String toJson() {
    return JSON.getGson().toJson(this);
  }
}
