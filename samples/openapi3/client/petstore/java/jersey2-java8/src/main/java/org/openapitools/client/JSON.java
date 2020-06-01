package org.openapitools.client;

import com.fasterxml.jackson.annotation.*;
import com.fasterxml.jackson.databind.*;
import org.openapitools.jackson.nullable.JsonNullableModule;
import com.fasterxml.jackson.datatype.jsr310.JavaTimeModule;
import org.openapitools.client.model.*;

import java.text.DateFormat;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;
import javax.ws.rs.ext.ContextResolver;


public class JSON implements ContextResolver<ObjectMapper> {
  private ObjectMapper mapper;

  public JSON() {
    mapper = new ObjectMapper();
    mapper.setSerializationInclusion(JsonInclude.Include.NON_NULL);
    mapper.configure(DeserializationFeature.FAIL_ON_UNKNOWN_PROPERTIES, true);
    mapper.configure(DeserializationFeature.FAIL_ON_INVALID_SUBTYPE, true);
    mapper.disable(SerializationFeature.WRITE_DATES_AS_TIMESTAMPS);
    mapper.enable(SerializationFeature.WRITE_ENUMS_USING_TO_STRING);
    mapper.enable(DeserializationFeature.READ_ENUMS_USING_TO_STRING);
    mapper.setDateFormat(new RFC3339DateFormat());
    mapper.registerModule(new JavaTimeModule());
    JsonNullableModule jnm = new JsonNullableModule();
    mapper.registerModule(jnm);
  }

  /**
   * Set the date format for JSON (de)serialization with Date properties.
   * @param dateFormat Date format
   */
  public void setDateFormat(DateFormat dateFormat) {
    mapper.setDateFormat(dateFormat);
  }

  @Override
  public ObjectMapper getContext(Class<?> type) {
    return mapper;
  }

  /**
   * Get the object mapper
   *
   * @return object mapper
   */
  public ObjectMapper getMapper() { return mapper; }

  /**
   * Returns the target model class that should be used to deserialize the input data.
   * The discriminator mappings are used to determine the target model class.
   *
   * @param node The input data.
   * @param modelClass The class that contains the discriminator mappings.
   */
  public static Class getClassForElement(JsonNode node, Class modelClass) {
    ClassDiscriminatorMapping cdm = modelDiscriminators.get(modelClass);
    if (cdm != null) {
      // Determine the value of the discriminator property in the input data.
      String discriminatorPropertyName = cdm.getPropertyName();
      if (discriminatorPropertyName != null) {
        // Get the value of the discriminator property, if present in the input payload.
        node = node.get(discriminatorPropertyName);
        if (node != null && node.isValueNode()) {
          String discrValue = node.asText();
          if (discrValue != null) {
            return cdm.getClassForElement(discrValue, new HashSet<Class>());
          }
        }
      }
    }
    return null;
  }

  /**
   * Helper class to register the discriminator mappings.
   */
  private static class ClassDiscriminatorMapping {
    // The model class name.
    Class modelClass;
    // The name of the discriminator property.
    String discriminatorName;
    // The discriminator mappings for a model class.
    Map<String, Class> discriminatorMappings;

    // Constructs a new class discriminator.
    ClassDiscriminatorMapping(Class cls, String name) {
      modelClass = cls;
      discriminatorName = name;
      discriminatorMappings = new HashMap<String, Class>();
    }

    // Register a discriminator mapping for the specified model class.
    void registerMapping(String mapping, Class cls) {
      discriminatorMappings.put(mapping, cls);
    }

    // Return the name of the discriminator property for this model class.
    String getPropertyName() {
      return discriminatorName;
    }

    /**
     * Returns the target model class that should be used to deserialize the input data.
     * This function can be invoked for anyOf/oneOf composed models with discriminator mappings.
     * The discriminator mappings are used to determine the target model class.
     *
     * @param discrValue The discriminator value
     * @param visitedClasses The set of classes that have already been visited.
     */
    Class getClassForElement(String discrValue, Set<Class> visitedClasses) {
      if (visitedClasses.contains(modelClass)) {
        // Class has already been visited.
        return null;
      }
      Class cls = discriminatorMappings.get(discrValue);
      if (cls != null) {
        return cls;
      }
      visitedClasses.add(modelClass);
      for (Class child : discriminatorMappings.values()) {
        ClassDiscriminatorMapping cdm = modelDiscriminators.get(child);
        if (cdm != null) {
          // Recursively traverse the discriminator mappings.
          cls = cdm.getClassForElement(discrValue, visitedClasses);
          if (cls != null) {
            return cls;
          }
        }
      }
      return null;
    }
  }

  private static Map<Class, ClassDiscriminatorMapping> modelDiscriminators = new HashMap<Class, ClassDiscriminatorMapping>();

  /**
   * Register the discriminators for all composed models.
   */
  private static void registerDiscriminators() {
    {
      // Initialize the discriminator mappings for 'Animal'.
      ClassDiscriminatorMapping m = new ClassDiscriminatorMapping(Animal.class, "className");
      m.registerMapping("Cat", Cat.class);
      m.registerMapping("Dog", Dog.class);
      m.registerMapping("Animal", Animal.class);
      modelDiscriminators.put(Animal.class, m);
    }
    {
      // Initialize the discriminator mappings for 'Cat'.
      ClassDiscriminatorMapping m = new ClassDiscriminatorMapping(Cat.class, "className");
      m.registerMapping("Cat", Cat.class);
      modelDiscriminators.put(Cat.class, m);
    }
    {
      // Initialize the discriminator mappings for 'ChildCat'.
      ClassDiscriminatorMapping m = new ClassDiscriminatorMapping(ChildCat.class, "pet_type");
      m.registerMapping("ChildCat", ChildCat.class);
      modelDiscriminators.put(ChildCat.class, m);
    }
    {
      // Initialize the discriminator mappings for 'Dog'.
      ClassDiscriminatorMapping m = new ClassDiscriminatorMapping(Dog.class, "className");
      m.registerMapping("Dog", Dog.class);
      modelDiscriminators.put(Dog.class, m);
    }
    {
      // Initialize the discriminator mappings for 'GrandparentAnimal'.
      ClassDiscriminatorMapping m = new ClassDiscriminatorMapping(GrandparentAnimal.class, "pet_type");
      m.registerMapping("ChildCat", ChildCat.class);
      m.registerMapping("ParentPet", ParentPet.class);
      m.registerMapping("GrandparentAnimal", GrandparentAnimal.class);
      modelDiscriminators.put(GrandparentAnimal.class, m);
    }
    {
      // Initialize the discriminator mappings for 'Mammal'.
      ClassDiscriminatorMapping m = new ClassDiscriminatorMapping(Mammal.class, "className");
      m.registerMapping("Pig", Pig.class);
      m.registerMapping("whale", Whale.class);
      m.registerMapping("zebra", Zebra.class);
      m.registerMapping("mammal", Mammal.class);
      modelDiscriminators.put(Mammal.class, m);
    }
    {
      // Initialize the discriminator mappings for 'NullableShape'.
      ClassDiscriminatorMapping m = new ClassDiscriminatorMapping(NullableShape.class, "shapeType");
      m.registerMapping("Quadrilateral", Quadrilateral.class);
      m.registerMapping("Triangle", Triangle.class);
      m.registerMapping("NullableShape", NullableShape.class);
      modelDiscriminators.put(NullableShape.class, m);
    }
    {
      // Initialize the discriminator mappings for 'ParentPet'.
      ClassDiscriminatorMapping m = new ClassDiscriminatorMapping(ParentPet.class, "pet_type");
      m.registerMapping("ChildCat", ChildCat.class);
      m.registerMapping("ParentPet", ParentPet.class);
      modelDiscriminators.put(ParentPet.class, m);
    }
    {
      // Initialize the discriminator mappings for 'Pig'.
      ClassDiscriminatorMapping m = new ClassDiscriminatorMapping(Pig.class, "className");
      m.registerMapping("BasquePig", BasquePig.class);
      m.registerMapping("DanishPig", DanishPig.class);
      m.registerMapping("Pig", Pig.class);
      modelDiscriminators.put(Pig.class, m);
    }
    {
      // Initialize the discriminator mappings for 'Quadrilateral'.
      ClassDiscriminatorMapping m = new ClassDiscriminatorMapping(Quadrilateral.class, "quadrilateralType");
      m.registerMapping("ComplexQuadrilateral", ComplexQuadrilateral.class);
      m.registerMapping("SimpleQuadrilateral", SimpleQuadrilateral.class);
      m.registerMapping("Quadrilateral", Quadrilateral.class);
      modelDiscriminators.put(Quadrilateral.class, m);
    }
    {
      // Initialize the discriminator mappings for 'Shape'.
      ClassDiscriminatorMapping m = new ClassDiscriminatorMapping(Shape.class, "shapeType");
      m.registerMapping("Quadrilateral", Quadrilateral.class);
      m.registerMapping("Triangle", Triangle.class);
      m.registerMapping("Shape", Shape.class);
      modelDiscriminators.put(Shape.class, m);
    }
    {
      // Initialize the discriminator mappings for 'ShapeOrNull'.
      ClassDiscriminatorMapping m = new ClassDiscriminatorMapping(ShapeOrNull.class, "shapeType");
      m.registerMapping("Quadrilateral", Quadrilateral.class);
      m.registerMapping("Triangle", Triangle.class);
      m.registerMapping("ShapeOrNull", ShapeOrNull.class);
      modelDiscriminators.put(ShapeOrNull.class, m);
    }
    {
      // Initialize the discriminator mappings for 'Triangle'.
      ClassDiscriminatorMapping m = new ClassDiscriminatorMapping(Triangle.class, "triangleType");
      m.registerMapping("EquilateralTriangle", EquilateralTriangle.class);
      m.registerMapping("IsoscelesTriangle", IsoscelesTriangle.class);
      m.registerMapping("ScaleneTriangle", ScaleneTriangle.class);
      m.registerMapping("Triangle", Triangle.class);
      modelDiscriminators.put(Triangle.class, m);
    }
  }

  static {
    registerDiscriminators();
  }

}
