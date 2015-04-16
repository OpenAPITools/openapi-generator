package com.wordnik.swagger.codegen.examples;

import com.wordnik.swagger.models.*;
import com.wordnik.swagger.models.properties.*;
import com.wordnik.swagger.util.Json;

import java.text.SimpleDateFormat;


import javax.xml.bind.JAXBContext;
import javax.xml.bind.JAXBException;
import javax.xml.bind.Marshaller;
 

import java.math.BigDecimal;
import java.util.*;

public class ExampleGenerator {
  protected Map<String, Model> examples;

  public ExampleGenerator(Map<String, Model> examples) {
    this.examples = examples;
  }

  public List<Map<String, String>> generate(Map<String, String> examples, List<String> mediaTypes, Property property) {
    List<Map<String, String>> output = new ArrayList<Map<String, String>>();
    Set<String> processedModels = new HashSet<String>();
    if(examples == null ) {
      if(mediaTypes == null) {
        // assume application/json for this
        mediaTypes = Arrays.asList("application/json");
      }
      for(String mediaType : mediaTypes) {
        Map<String, String> kv = new HashMap<String, String>();
        kv.put("contentType", mediaType);
        if(property != null && mediaType.startsWith("application/json")) {
          String example = Json.pretty(resolvePropertyToExample(mediaType, property, processedModels));

          if(example != null) {
            kv.put("example", example);
            output.add(kv);
          }
        }
        else if(property != null && mediaType.startsWith("application/xml")) {
          String example = new XmlExampleGenerator(this.examples).toXml(property);
          if(example != null) {
            kv.put("example", example);
            output.add(kv);
          }
        }
      }
    }
    else {
      for(String key: examples.keySet()) {
        String value = examples.get(key);

        Map<String, String> kv = new HashMap<String, String>();
        kv.put("contentType", key);
        kv.put("example", value);
        output.add(kv);
      }
    }
    if(output.size() == 0) {
      Map<String, String> kv = new HashMap<String, String>();
      kv.put("output", "none");
      output.add(kv);
    }
    return output;
  }

  protected Object resolvePropertyToExample(String mediaType, Property property, Set<String> processedModels) {
    if(property.getExample() != null) {
      return property.getExample();
    }
    else if(property instanceof StringProperty) {
      return "aeiou";
    }
    else if(property instanceof BooleanProperty) {
      return Boolean.TRUE;
    }
    else if(property instanceof ArrayProperty) {
      Property innerType = ((ArrayProperty)property).getItems();
      if(innerType != null) {
        Object[] output = new Object[]{
          resolvePropertyToExample(mediaType, innerType, processedModels)
        };
        return output;
      }
    }
    else if(property instanceof DateProperty) {
      return new java.util.Date(System.currentTimeMillis());
    }
    else if(property instanceof DateTimeProperty) {
      return new java.util.Date(System.currentTimeMillis());
    }
    else if(property instanceof DecimalProperty) {
      return new BigDecimal(1.3579);
    }
    else if(property instanceof DoubleProperty) {
      return new Double(3.149);
    }
    else if(property instanceof FileProperty) {
      return "";  // TODO
    }
    else if(property instanceof FloatProperty) {
      return new Float(1.23);
    }
    else if(property instanceof IntegerProperty) {
      return new Integer(123);
    }
    else if(property instanceof LongProperty) {
      return new Long(123456789);
    }
    else if(property instanceof MapProperty) {
      Map<String, Object> mp = new HashMap<String, Object>();
      if(property.getName() != null) {
        mp.put(property.getName(),
          resolvePropertyToExample(mediaType, ((MapProperty)property).getAdditionalProperties(), processedModels));
      }
      else {
        mp.put("key",
          resolvePropertyToExample(mediaType, ((MapProperty)property).getAdditionalProperties(), processedModels));
      }
      return mp;
    }
    else if(property instanceof ObjectProperty) {
      return "{}";
    }
    else if(property instanceof RefProperty) {
      String simpleName = ((RefProperty)property).getSimpleRef();
      Model model = examples.get(simpleName);
      if(model != null)
        return resolveModelToExample(simpleName, mediaType, model, processedModels);
    }
    else if(property instanceof UUIDProperty) {
      return "046b6c7f-0b8a-43b9-b35d-6489e6daee91";
    }

    return "";
  }

  public Object resolveModelToExample(String name, String mediaType, Model model, Set<String> processedModels) {
    if(processedModels.contains(name)) {
      return "";
    }
    if(model instanceof ModelImpl) {
      processedModels.add(name);
      ModelImpl impl = (ModelImpl) model;
      Map<String, Object> values = new HashMap<String, Object>();

      if(impl != null && impl.getProperties() != null) {
        for(String propertyName : impl.getProperties().keySet()) {
          Property property = impl.getProperties().get(propertyName);
          values.put(propertyName, resolvePropertyToExample(mediaType, property, processedModels));
        }
      }

      return values;
    }

    return "";
  }
}