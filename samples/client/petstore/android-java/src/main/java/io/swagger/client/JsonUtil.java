package io.swagger.client;

import com.google.gson.Gson;
import com.google.gson.GsonBuilder;

public class JsonUtil {
  public static GsonBuilder gsonBuilder;

  static {
    gsonBuilder = new GsonBuilder();
    gsonBuilder.serializeNulls();
  }

  public static Gson getGson() {
    return gsonBuilder.create();
  }
};