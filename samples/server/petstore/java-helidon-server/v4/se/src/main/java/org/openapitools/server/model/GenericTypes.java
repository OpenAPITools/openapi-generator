package org.openapitools.server.model;

import java.util.List;
import java.util.Map;

import io.helidon.common.GenericType;

public interface GenericTypes {
    GenericType<List<EnumClass>> TYPE__List_EnumClass = new GenericType<>() {};
    GenericType<List<String>> TYPE__List_String = new GenericType<>() {};
    GenericType<List<User>> TYPE__List_User = new GenericType<>() {};
    GenericType<Map<String, Object>> TYPE__Map_Object = new GenericType<>() {};
    GenericType<Map<String, String>> TYPE__Map_String = new GenericType<>() {};
}
