package io.swagger.client;

import com.google.gson.Gson;
import com.google.gson.GsonBuilder;
import com.google.gson.reflect.TypeToken;
import java.lang.reflect.Type;
import java.util.List;
import io.swagger.client.model.*;

public class JsonUtil {
  public static GsonBuilder gsonBuilder;

  static {
    gsonBuilder = new GsonBuilder();
    gsonBuilder.serializeNulls();
    gsonBuilder.setDateFormat("yyyy-MM-dd'T'HH:mm:ss.SSSZ");
  }

  public static Gson getGson() {
    return gsonBuilder.create();
  }

  public static String serialize(Object obj){
    return getGson().toJson(obj);
  }

  public static <T> T deserializeToList(String jsonString, Class cls){
    return getGson().fromJson(jsonString, getListTypeForDeserialization(cls));
  }

  public static <T> T deserializeToObject(String jsonString, Class cls){
    return getGson().fromJson(jsonString, getTypeForDeserialization(cls));
  }

  public static Type getListTypeForDeserialization(Class cls) {
    Type type = null;
    switch (cls.getSimpleName()) {
      
      case "User":
        type = new TypeToken<List<User>>(){}.getType();
        break;
      
      case "Category":
        type = new TypeToken<List<Category>>(){}.getType();
        break;
      
      case "Pet":
        type = new TypeToken<List<Pet>>(){}.getType();
        break;
      
      case "Tag":
        type = new TypeToken<List<Tag>>(){}.getType();
        break;
      
      case "Order":
        type = new TypeToken<List<Order>>(){}.getType();
        break;
      
      default:
        type = new TypeToken<List<Object>>(){}.getType();
        break;
    }
    return type;
  }

  public static Type getTypeForDeserialization(Class cls) {
    Type type = null;
    switch (cls.getSimpleName()) {
      
      case "User":
        type = new TypeToken<User>(){}.getType();
      break;
      
      case "Category":
        type = new TypeToken<Category>(){}.getType();
      break;
      
      case "Pet":
        type = new TypeToken<Pet>(){}.getType();
      break;
      
      case "Tag":
        type = new TypeToken<Tag>(){}.getType();
      break;
      
      case "Order":
        type = new TypeToken<Order>(){}.getType();
      break;
      
      default:
        type = new TypeToken<Object>(){}.getType();
        break;
    }
    return type;
  }

};
