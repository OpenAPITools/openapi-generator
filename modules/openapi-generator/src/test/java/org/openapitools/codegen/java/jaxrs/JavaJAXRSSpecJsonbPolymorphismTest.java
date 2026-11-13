/*
 * Copyright 2026 OpenAPI-Generator Contributors (https://openapi-generator.tech)
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package org.openapitools.codegen.java.jaxrs;

import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.databind.ObjectMapper;
import jakarta.json.bind.Jsonb;
import jakarta.json.bind.JsonbBuilder;
import jakarta.json.bind.spi.JsonbProvider;
import jakarta.json.stream.JsonParser;
import org.openapitools.codegen.CodegenConstants;
import org.openapitools.codegen.TestUtils;
import org.openapitools.codegen.languages.AbstractJavaJAXRSServerCodegen;
import org.openapitools.codegen.languages.JavaJAXRSCXFCDIServerCodegen;
import org.openapitools.codegen.languages.JavaJAXRSSpecServerCodegen;
import org.testng.SkipException;
import org.testng.annotations.DataProvider;
import org.testng.annotations.Test;

import java.io.IOException;
import java.io.UncheckedIOException;
import java.net.URL;
import java.net.URLClassLoader;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Enumeration;
import java.util.HashMap;
import java.util.LinkedHashSet;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.concurrent.Callable;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import static org.openapitools.codegen.TestUtils.assertFileContains;
import static org.openapitools.codegen.TestUtils.assertFileNotContains;
import static org.openapitools.codegen.java.jaxrs.JavaJAXRSSpecCompileSupport.MODEL_PACKAGE;
import static org.openapitools.codegen.java.jaxrs.JavaJAXRSSpecCompileSupport.SPEC_DIR;
import static org.openapitools.codegen.java.jaxrs.JavaJAXRSSpecCompileSupport.assertCompiles;
import static org.openapitools.codegen.java.jaxrs.JavaJAXRSSpecCompileSupport.classpath;
import static org.openapitools.codegen.java.jaxrs.JavaJAXRSSpecCompileSupport.compileModels;
import static org.openapitools.codegen.java.jaxrs.JavaJAXRSSpecCompileSupport.generate;
import static org.openapitools.codegen.java.jaxrs.JavaJAXRSSpecCompileSupport.loader;
import static org.openapitools.codegen.java.jaxrs.JavaJAXRSSpecCompileSupport.modelDir;
import static org.openapitools.codegen.java.jaxrs.JavaJAXRSSpecCompileSupport.requireJdk;
import static org.openapitools.codegen.java.jaxrs.JavaJAXRSSpecCompileSupport.writeSource;
import static org.testng.Assert.assertEquals;
import static org.testng.Assert.assertTrue;
import static org.testng.Assert.expectThrows;

/**
 * Tests the {@code jsonbDiscriminatorDeserializers} option of the jaxrs-spec generator: JSON-B deserializers that resolve the concrete
 * class of a polymorphic model from its discriminator, without adding classes to the (sealed) model hierarchy. The
 * runtime tests compile the generated models for Java 17 and bind them with the Yasson (Jakarta JSON-B 3.0) of the test
 * classpath. Every generation runs with {@code legacyDiscriminatorBehavior} {@code true} and {@code false}.
 */
public class JavaJAXRSSpecJsonbPolymorphismTest {

    private static final String SPEC = SPEC_DIR + "jsonb_polymorphism.yaml";
    private static final String DESERIALIZER = "@jakarta.json.bind.annotation.JsonbTypeDeserializer(" + MODEL_PACKAGE + ".";
    private static final String DEFAULTS_SPEC = SPEC_DIR + "jsonb_polymorphism_defaults.yaml";
    private static final String CONTAINERS_SPEC = SPEC_DIR + "jsonb_polymorphism_containers.yaml";
    /** the result of a missing creator parameter with a deserializer under Johnzon (a bug of Johnzon 2.0.2 to 2.2.0) */
    private static final String JOHNZON_MISSING_CREATOR_PARAMETER = "Johnzon: NullPointerException before the deserializer";

    @DataProvider
    public Object[][] legacyDiscriminatorBehavior() {
        return new Object[][]{{true}, {false}};
    }

    /**
     * Only the polymorphic types get a deserializer, and only a concrete type that a value maps to itself gets the
     * transfer object JsonbData. Concrete models are created with their discriminator values, the JSON-B creator is
     * unchanged. Without the option, nothing of this is generated and the class declarations are the same.
     */
    @Test(dataProvider = "legacyDiscriminatorBehavior")
    public void testGeneratedDeserializers(boolean legacy) throws Exception {
        Path modelDir = modelDir(generate(SPEC, properties(legacy)));
        assertFileContains(modelDir.resolve("Pet.java"),
                DESERIALIZER + "Pet.JsonbPolymorphicDeserializer.class)",
                "public abstract sealed class Pet permits Cat, Dog {",
                "protected Pet() { }");
        assertFileContains(modelDir.resolve("Cat.java"),
                DESERIALIZER + "Cat.JsonbPolymorphicDeserializer.class)",
                "public abstract sealed class Cat extends Pet permits PersianCat, SiameseCat {");
        assertFileContains(modelDir.resolve("Vehicle.java"),
                DESERIALIZER + "Vehicle.JsonbPolymorphicDeserializer.class)",
                "public sealed class Vehicle permits Car {",
                "public static final class JsonbData {",
                "public Vehicle() { this.setKind(\"VEHICLE\"); }");
        assertFileContains(modelDir.resolve("Car.java"), DESERIALIZER + "Car.JsonbPolymorphicDeserializer.class)",
                "public static final class JsonbData {");
        assertFileContains(modelDir.resolve("SportsCar.java"),
                "public SportsCar() { this.setCarType(\"SPORTS\"); this.setKind(\"CAR\"); }",
                "{ // the discriminator values of SportsCar, which the caller may override this.carType(\"SPORTS\"); this.kind(\"CAR\"); }");
        assertFileContains(modelDir.resolve("PersianCat.java"), "public PersianCat() { this.setPetType(\"PERSIAN\"); }");
        assertFileContains(modelDir.resolve("Dog.java"), "public Dog() { this.setPetType(\"DOG\"); }",
                "@JsonbCreator public Dog( @JsonbProperty(\"size\") SizeEnum size, @JsonbProperty(\"petType\") String petType, "
                        + "@JsonbProperty(\"name\") String name ) { super( petType, name ); this.size = size; }");
        for (String model : Arrays.asList("Pet.java", "Cat.java")) {
            assertFileNotContains(modelDir.resolve(model), "JsonbData");
        }
        for (String model : Arrays.asList("Dog.java", "PersianCat.java", "SiameseCat.java", "SportsCar.java", "Holder.java")) {
            // (enum properties carry their own @JsonbTypeDeserializer, as without the option)
            assertFileNotContains(modelDir.resolve(model), "JsonbPolymorphicDeserializer", "JsonbData");
        }

        Map<String, Object> withoutOption = properties(legacy);
        withoutOption.remove(JavaJAXRSSpecServerCodegen.JSONB_DISCRIMINATOR_DESERIALIZERS);
        Path plainDir = modelDir(generate(SPEC, withoutOption));
        try (Stream<Path> models = Files.list(plainDir)) {
            for (Path model : models.collect(Collectors.toList())) {
                assertFileNotContains(model, "JsonbPolymorphicDeserializer", "JsonbPolymorphicContainerDeserializer", "JsonbData", "jakarta.json.spi", "discriminator values of",
                        "this.setKind(", "this.setPetType(", "this.setCarType(");
                assertEquals(classDeclaration(modelDir.resolve(model.getFileName())), classDeclaration(model), model.toString());
            }
        }
        assertFileContains(plainDir.resolve("SportsCar.java"), "public SportsCar() { }");
    }

    @Test
    public void testOptionIsAdvertisedByJaxrsSpecOnly() {
        assertEquals(JavaJAXRSSpecServerCodegen.JSONB_DISCRIMINATOR_DESERIALIZERS, "jsonbDiscriminatorDeserializers");
        assertTrue(new JavaJAXRSSpecServerCodegen().cliOptions().stream()
                .anyMatch(option -> JavaJAXRSSpecServerCodegen.JSONB_DISCRIMINATOR_DESERIALIZERS.equals(option.getOpt())));
        // cxf-cdi always uses Jackson and its own templates
        assertTrue(new JavaJAXRSCXFCDIServerCodegen().cliOptions().stream()
                .noneMatch(option -> JavaJAXRSSpecServerCodegen.JSONB_DISCRIMINATOR_DESERIALIZERS.equals(option.getOpt())));
    }

    @Test
    public void testRequiresJakarta() {
        Map<String, Object> properties = properties(true);
        properties.put(AbstractJavaJAXRSServerCodegen.USE_JAKARTA_EE, "false");
        IllegalArgumentException e = expectThrows(IllegalArgumentException.class, () -> generate(SPEC, properties));
        assertTrue(e.getMessage().contains("'jsonbDiscriminatorDeserializers=true' requires 'useJakartaEe=true'"), e.getMessage());
    }

    @Test
    public void testIgnoredWithJackson() throws Exception {
        Map<String, Object> properties = properties(true);
        properties.remove(CodegenConstants.SERIALIZATION_LIBRARY);
        Path modelDir = modelDir(generate(SPEC, properties));
        try (Stream<Path> models = Files.list(modelDir)) {
            for (Path model : models.collect(Collectors.toList())) {
                assertFileNotContains(model, "jakarta.json.bind", "JsonbPolymorphicDeserializer", "JsonbPolymorphicContainerDeserializer");
            }
        }
        assertFileContains(modelDir.resolve("Pet.java"), "@JsonSubTypes.Type(value = PersianCat.class, name = \"PERSIAN\")");
    }

    /**
     * Polymorphic deserialization through abstract and concrete roots, an abstract intermediate type, a second
     * discriminator level, lists, maps and fields (also of a non-polymorphic holder), JSON-B creators with enums,
     * builders, and Java -> JSON -> Java roundtrips; models created by their constructor or builder without setting a
     * discriminator value are written with it and read back as themselves.
     */
    @Test(dataProvider = "legacyDiscriminatorBehavior")
    public void testPolymorphicBinding(boolean legacy) throws Exception {
        requireJdk(17);
        Map<String, String> results = runChecks(generate(SPEC, properties(legacy)), "JsonbChecks",
                "  static final String DEEP = \"{\\\"petType\\\":\\\"DOG\\\",\\\"name\\\":\\\"rex\\\",\\\"size\\\":\\\"large\\\","
                        + "\\\"friends\\\":[{\\\"petType\\\":\\\"PERSIAN\\\",\\\"name\\\":\\\"tom\\\",\\\"longHair\\\":true},null,"
                        + "{\\\"petType\\\":\\\"DOG\\\",\\\"name\\\":\\\"d2\\\",\\\"size\\\":\\\"small\\\"}],"
                        + "\\\"friendsByName\\\":{\\\"s\\\":{\\\"petType\\\":\\\"SIAMESE\\\",\\\"name\\\":\\\"s\\\",\\\"eyeColor\\\":\\\"blue\\\"}},"
                        + "\\\"best\\\":{\\\"petType\\\":\\\"PERSIAN\\\",\\\"name\\\":\\\"b\\\"}}\";",
                "  static final String HOLDER = \"{\\\"pet\\\":{\\\"petType\\\":\\\"SIAMESE\\\",\\\"name\\\":\\\"a\\\"},"
                        + "\\\"cat\\\":{\\\"petType\\\":\\\"PERSIAN\\\",\\\"name\\\":\\\"c\\\"},"
                        + "\\\"pets\\\":[{\\\"petType\\\":\\\"DOG\\\",\\\"name\\\":\\\"b\\\",\\\"size\\\":\\\"small\\\"}],"
                        + "\\\"petsByName\\\":{\\\"x\\\":{\\\"petType\\\":\\\"PERSIAN\\\",\\\"name\\\":\\\"x\\\"}},"
                        + "\\\"vehicles\\\":[{\\\"kind\\\":\\\"CAR\\\",\\\"carType\\\":\\\"CAR\\\",\\\"doors\\\":2},{\\\"kind\\\":\\\"VEHICLE\\\"}]}\";",
                "  static void checks() throws Exception {",
                "    check(\"Pet<-PERSIAN\", j -> cls(j.fromJson(\"{\\\"petType\\\":\\\"PERSIAN\\\",\\\"name\\\":\\\"tom\\\",\\\"longHair\\\":true}\", Pet.class)));",
                "    check(\"Pet<-DOG creator\", j -> { Dog d = (Dog) j.fromJson(\"{\\\"petType\\\":\\\"DOG\\\",\\\"name\\\":\\\"rex\\\",\\\"size\\\":\\\"large\\\"}\", Pet.class); return d.getSize() + \"/\" + d.getName(); });",
                "    check(\"Cat<-SIAMESE\", j -> cls(j.fromJson(\"{\\\"petType\\\":\\\"SIAMESE\\\",\\\"name\\\":\\\"s\\\"}\", Cat.class)));",
                "    check(\"Cat<-DOG\", j -> cls(j.fromJson(\"{\\\"petType\\\":\\\"DOG\\\",\\\"name\\\":\\\"s\\\",\\\"size\\\":\\\"small\\\"}\", Cat.class)));",
                "    check(\"Pet<-unknown\", j -> cls(j.fromJson(\"{\\\"petType\\\":\\\"FOX\\\"}\", Pet.class)));",
                "    check(\"Pet<-missing\", j -> cls(j.fromJson(\"{\\\"name\\\":\\\"x\\\"}\", Pet.class)));",
                "    check(\"Pet<-null\", j -> cls(j.fromJson(\"null\", Pet.class)));",
                "    check(\"Pet<-array\", j -> cls(j.fromJson(\"[1]\", Pet.class)));",
                "    check(\"PersianCat direct\", j -> cls(j.fromJson(\"{\\\"petType\\\":\\\"PERSIAN\\\",\\\"name\\\":\\\"tom\\\"}\", PersianCat.class)));",
                "    check(\"Vehicle<-VEHICLE\", j -> { Vehicle v = j.fromJson(\"{\\\"kind\\\":\\\"VEHICLE\\\",\\\"fuel\\\":\\\"electric\\\"}\", Vehicle.class); return cls(v) + \" \" + v.getFuel() + \" \" + v.getWheels(); });",
                "    check(\"Vehicle<-CAR/CAR\", j -> { Vehicle v = j.fromJson(\"{\\\"kind\\\":\\\"CAR\\\",\\\"carType\\\":\\\"CAR\\\",\\\"doors\\\":4,\\\"fuel\\\":\\\"petrol\\\"}\", Vehicle.class); return cls(v) + \" \" + ((Car) v).getDoors() + \" \" + v.getFuel(); });",
                "    check(\"Vehicle<-CAR/SPORTS\", j -> { Vehicle v = j.fromJson(\"{\\\"kind\\\":\\\"CAR\\\",\\\"carType\\\":\\\"SPORTS\\\",\\\"horsePower\\\":300}\", Vehicle.class); return cls(v) + \" \" + ((SportsCar) v).getHorsePower(); });",
                "    check(\"Vehicle<-CAR/missing\", j -> cls(j.fromJson(\"{\\\"kind\\\":\\\"CAR\\\"}\", Vehicle.class)));",
                "    check(\"Car<-VEHICLE\", j -> cls(j.fromJson(\"{\\\"kind\\\":\\\"VEHICLE\\\",\\\"carType\\\":\\\"VEHICLE\\\"}\", Car.class)));",
                "    check(\"deep types\", j -> { Dog d = (Dog) j.fromJson(DEEP, Pet.class); return d.getFriends().stream().map(JsonbChecks::cls).collect(java.util.stream.Collectors.toList()) + \" \" + cls(d.getFriendsByName().get(\"s\")) + \" \" + cls(d.getBest()); });",
                "    check(\"deep roundtrip\", j -> { Pet p = j.fromJson(DEEP, Pet.class); return p.equals(j.fromJson(j.toJson(p), Pet.class)); });",
                "    check(\"builder roundtrip\", j -> { PersianCat c = PersianCat.builder().petType(\"PERSIAN\").name(\"tom\").longHair(true).build(); return c.equals(j.fromJson(j.toJson(c), Pet.class)); });",
                "    check(\"builder without discriminator\", j -> { PersianCat c = PersianCat.builder().name(\"tom\").build(); Pet p = j.fromJson(j.toJson(c), Pet.class); return c.getPetType() + \" \" + cls(p) + \" \" + c.equals(p); });",
                "    check(\"builder overrides discriminator\", j -> PersianCat.builder().petType(\"SIAMESE\").build().getPetType() + \" \" + PersianCat.builder().petType(null).build().getPetType());",
                "    check(\"SportsCar builder\", j -> { SportsCar s = SportsCar.builder().horsePower(300).build(); Vehicle v = j.fromJson(j.toJson(s), Vehicle.class); return s.getKind() + \"/\" + s.getCarType() + \" \" + cls(v) + \" \" + s.equals(v); });",
                "    check(\"Car builder\", j -> { Car c = Car.builder().doors(2).build(); Vehicle v = j.fromJson(j.toJson(c), Vehicle.class); return c.getKind() + \"/\" + c.getCarType() + \" \" + cls(v) + \" \" + c.equals(v); });",
                "    check(\"constructors\", j -> new Vehicle().getKind() + \" \" + new Car().getKind() + \"/\" + new Car().getCarType() + \" \" + new SportsCar().getKind() + \"/\" + new SportsCar().getCarType()"
                        + " + \" \" + new Dog().getPetType() + \" \" + new PersianCat().getPetType() + \" \" + new SiameseCat().getPetType());",
                "    check(\"constructor roundtrip\", j -> { SportsCar s = new SportsCar(); s.setHorsePower(1); Dog d = new Dog(); d.setName(\"rex\"); d.addFriendsItem(s.getHorsePower() == 1 ? new SiameseCat() : null);"
                        + " Pet p = j.fromJson(j.toJson(d), Pet.class); Vehicle v = j.fromJson(j.toJson(s), Vehicle.class); return cls(p) + \" \" + d.equals(p) + \" \" + cls(v) + \" \" + s.equals(v); });",
                "    check(\"creator unchanged\", j -> { Dog d = j.fromJson(\"{\\\"name\\\":\\\"rex\\\",\\\"size\\\":\\\"small\\\"}\", Dog.class); return d.getPetType() + \" \" + d.getName(); });",
                // the root containers need the workaround of testYassonContainerLimitation (the fields of Holder do not)
                "    check(\"List<Pet>\", j -> { readTypesFirst(j, Pet.class); java.lang.reflect.Type t = new java.util.ArrayList<Pet>() { }.getClass().getGenericSuperclass();"
                        + " java.util.List<Pet> l = j.fromJson(\"[{\\\"petType\\\":\\\"SIAMESE\\\",\\\"name\\\":\\\"a\\\"},{\\\"petType\\\":\\\"DOG\\\",\\\"name\\\":\\\"b\\\",\\\"size\\\":\\\"small\\\"}]\", t);"
                        + " return cls(l.get(0)) + \",\" + cls(l.get(1)) + \" \" + l.equals(j.fromJson(j.toJson(l), t)); });",
                "    check(\"Map<String,Vehicle>\", j -> { readTypesFirst(j, Vehicle.class); java.lang.reflect.Type t = new java.util.HashMap<String, Vehicle>() { }.getClass().getGenericSuperclass();"
                        + " java.util.Map<String, Vehicle> m = j.fromJson(\"{\\\"a\\\":{\\\"kind\\\":\\\"CAR\\\",\\\"carType\\\":\\\"SPORTS\\\"},\\\"b\\\":{\\\"kind\\\":\\\"VEHICLE\\\"}}\", t);"
                        + " return cls(m.get(\"a\")) + \",\" + cls(m.get(\"b\")) + \" \" + m.equals(j.fromJson(j.toJson(m), t)); });",
                "    check(\"Holder fields\", j -> { Holder h = j.fromJson(HOLDER, Holder.class); return cls(h.getPet()) + \" \" + cls(h.getCat()) + \" \" + cls(h.getPets().get(0)) + \" \" + cls(h.getPetsByName().get(\"x\")) + \" \" + cls(h.getVehicles().get(0)) + \",\" + cls(h.getVehicles().get(1)); });",
                "    check(\"Holder roundtrip\", j -> { Holder h = j.fromJson(HOLDER, Holder.class); return h.equals(j.fromJson(j.toJson(h), Holder.class)); });",
                "    check(\"Vehicle self roundtrip\", j -> { Vehicle v = j.fromJson(\"{\\\"kind\\\":\\\"VEHICLE\\\",\\\"fuel\\\":\\\"electric\\\",\\\"wheels\\\":3}\", Vehicle.class); return v.equals(j.fromJson(j.toJson(v), Vehicle.class)); });",
                "  }");

        Map<String, String> expected = new LinkedHashMap<>();
        expected.put("Pet<-PERSIAN", "PersianCat");
        expected.put("Pet<-DOG creator", "large/rex");
        expected.put("Cat<-SIAMESE", "SiameseCat");
        expected.put("Cat<-DOG", "JsonbException: Unknown or missing discriminator value for 'petType' of Cat: DOG");
        expected.put("Pet<-unknown", "JsonbException: Unknown or missing discriminator value for 'petType' of Pet: FOX");
        expected.put("Pet<-missing", "JsonbException: Unknown or missing discriminator value for 'petType' of Pet: null");
        expected.put("Pet<-null", "null");
        expected.put("Pet<-array", "JsonbException: Expected a JSON object for Pet but got ARRAY");
        expected.put("PersianCat direct", "PersianCat");
        expected.put("Vehicle<-VEHICLE", "Vehicle electric 4");
        expected.put("Vehicle<-CAR/CAR", "Car 4 petrol");
        expected.put("Vehicle<-CAR/SPORTS", "SportsCar 300");
        expected.put("Vehicle<-CAR/missing", "Car");
        expected.put("Car<-VEHICLE", "JsonbException: Unknown or missing discriminator value for 'carType' of Car: VEHICLE");
        expected.put("deep types", "[PersianCat, null, Dog] SiameseCat PersianCat");
        expected.put("deep roundtrip", "true");
        expected.put("builder roundtrip", "true");
        expected.put("builder without discriminator", "PERSIAN PersianCat true");
        expected.put("builder overrides discriminator", "SIAMESE null");
        expected.put("SportsCar builder", "CAR/SPORTS SportsCar true");
        expected.put("Car builder", "CAR/CAR Car true");
        expected.put("constructors", "VEHICLE CAR/CAR CAR/SPORTS DOG PERSIAN SIAMESE");
        expected.put("constructor roundtrip", "Dog true SportsCar true");
        expected.put("creator unchanged", "null rex");
        expected.put("List<Pet>", "SiameseCat,Dog true");
        expected.put("Map<String,Vehicle>", "SportsCar,Vehicle true");
        expected.put("Holder fields", "SiameseCat PersianCat Dog PersianCat Car,Vehicle");
        expected.put("Holder roundtrip", "true");
        expected.put("Vehicle self roundtrip", "true");
        // Johnzon fails to create every Dog, also without the option: 2.0 ignores the deserializer of the enum Dog.SizeEnum at
        // the creator parameter, 2.2 calls it with a parser that does not support next() (and fails if size is missing);
        // Johnzon ignores that of FuelEnum at the field of the transfer object;
        String dogLarge = "JsonbException: Dog cannot be constructed to deserialize json object value: {\"petType\":\"DOG\",\"name\":\"rex\",\"size\":\"large\"";
        expectUnderJohnzon(expected, "Pet<-DOG creator", dogLarge + "}");
        expectUnderJohnzon(expected, "deep types", dogLarge + ",\"frie...");
        expectUnderJohnzon(expected, "deep roundtrip", dogLarge + ",\"frie...");
        expectUnderJohnzon(expected, "List<Pet>", "JsonbException: Dog cannot be constructed to deserialize json object value: {\"petType\":\"DOG\",\"name\":\"b\",\"size\":\"small\"}");
        String holderDog = "JsonbException: Holder property 'pets' of type List<Pet> cannot be mapped to json array value: [{\"petType\":\"DOG\",\"name\":\"b\",\"size\":\"small\"}]";
        expectUnderJohnzon(expected, "Holder fields", holderDog);
        expectUnderJohnzon(expected, "Holder roundtrip", holderDog);
        expectUnderJohnzon(expected, "2.2.", "constructor roundtrip", "JsonbException: Dog cannot be constructed to deserialize json object value: "
                + "{\"name\":\"rex\",\"petType\":\"DOG\",\"friends\":[{\"petType...");
        expectUnderJohnzon(expected, "Vehicle<-VEHICLE", "JsonbException: JsonbData property 'fuel' of type FuelEnum cannot be mapped to json string value: \"electric\"");
        expectUnderJohnzon(expected, "Vehicle<-CAR/CAR", "JsonbException: JsonbData property 'fuel' of type FuelEnum cannot be mapped to json string value: \"petrol\"");
        expectUnderJohnzon(expected, "Vehicle self roundtrip", "JsonbException: JsonbData property 'fuel' of type FuelEnum cannot be mapped to json string value: \"electric\"");
        // it reads a Dog with the deserializer of Pet, and rejects an array before calling that
        expectUnderJohnzon(expected, "creator unchanged", "JsonbException: Unknown or missing discriminator value for 'petType' of Pet: null");
        expectUnderJohnzon(expected, "Pet<-array", "IllegalArgumentException: class " + MODEL_PACKAGE + ".Pet does not support json array value: [1]");
        assertResults(results, expected);
    }

    /**
     * Only lists, sets and maps (also nested ones) of models with a deserializer get the container deserializer of that
     * model, at the setter, the creator parameter (also an inherited one) and the field of the transfer object; only the
     * models used so get it. Without the option, nothing of this is generated.
     */
    @Test
    public void testGeneratedContainerDeserializers() throws Exception {
        String pets = DESERIALIZER + "Pet.JsonbPolymorphicContainerDeserializer.class)";
        String cats = DESERIALIZER + "Cat.JsonbPolymorphicContainerDeserializer.class)";
        String vehicles = DESERIALIZER + "Vehicle.JsonbPolymorphicContainerDeserializer.class)";
        Path modelDir = modelDir(generate(CONTAINERS_SPEC, properties(false)));
        Map<String, List<String>> expected = new LinkedHashMap<>();
        expected.put("Containers.java", Arrays.asList(pets, pets, pets, pets, pets, pets, pets, cats, vehicles, vehicles));
        expected.put("Required.java", Arrays.asList(pets, vehicles, pets, vehicles));
        // the creator of House and Shelter, and the setter of House (inherited by Shelter) and of the transfer object
        expected.put("House.java", Arrays.asList(pets, pets, pets));
        expected.put("Shelter.java", Arrays.asList(pets));
        expected.put("Dog.java", Arrays.asList(pets, pets));
        expected.put("Vehicle.java", Arrays.asList(vehicles, vehicles));
        expected.put("Pet.java", Collections.emptyList());
        expected.put("Cat.java", Collections.emptyList());
        expected.put("Car.java", Collections.emptyList());
        expected.put("PersianCat.java", Collections.emptyList());
        expected.forEach((model, annotations) -> {
            List<String> found = Arrays.stream(TestUtils.linearize(readModel(modelDir.resolve(model))).split("(?=@jakarta)"))
                    .filter(part -> part.contains("ContainerDeserializer.class)"))
                    .map(part -> part.substring(0, part.indexOf(".class)")) + ".class)")
                    .collect(Collectors.toList());
            assertEquals(found, annotations, model);
        });
        // not annotated: Map<String, String>, List<List<String>> and containers of models without a deserializer
        assertFileContains(modelDir.resolve("Containers.java"),
                "@JsonbProperty(\"stringMap\") public void setStringMap(Map<String, String> stringMap)",
                "@JsonbProperty(\"stringLists\") public void setStringLists(List<List<String>> stringLists)",
                "@JsonbProperty(\"carMap\") public void setCarMap(Map<String, Car> carMap)",
                "@JsonbProperty(\"persianMap\") public void setPersianMap(Map<String, PersianCat> persianMap)");
        for (String model : Arrays.asList("Pet.java", "Cat.java", "Vehicle.java")) {
            assertFileContains(modelDir.resolve(model),
                    "public static final class JsonbPolymorphicContainerDeserializer implements jakarta.json.bind.serializer.JsonbDeserializer<Object> {");
        }
        for (String model : Arrays.asList("House.java", "Shelter.java", "Car.java", "Dog.java", "PersianCat.java", "Containers.java", "Required.java")) {
            assertFileNotContains(modelDir.resolve(model), "class JsonbPolymorphicContainerDeserializer");
        }

        Map<String, Object> withoutOption = properties(false);
        withoutOption.remove(JavaJAXRSSpecServerCodegen.JSONB_DISCRIMINATOR_DESERIALIZERS);
        try (Stream<Path> models = Files.list(modelDir(generate(CONTAINERS_SPEC, withoutOption)))) {
            for (Path model : models.collect(Collectors.toList())) {
                assertFileNotContains(model, "ContainerDeserializer", "JsonbPolymorphicDeserializer");
            }
        }
    }

    /**
     * Containers of polymorphic models in fields and creator parameters (also inherited ones) of polymorphic and other
     * models, with fresh Jsonb instances and without reading the types first (also a subclass read directly, before its
     * root type): the elements keep their classes and the fields of their subclasses, also when a root container was
     * read before (which Yasson up to 3.0.4 binds wrongly and caches), in either order and repeatedly; missing and null
     * containers and elements, wrong JSON types and unknown or missing discriminator values. With Johnzon (2.0.2 to
     * 2.2.0), a missing creator parameter with a deserializer fails before the deserializer is called.
     */
    @Test(dataProvider = "legacyDiscriminatorBehavior")
    public void testContainerBinding(boolean legacy) throws Exception {
        requireJdk(17);
        String allJson = "{'petList':[$P,$D],'petSet':[$P],'petMap':{'p':$P,'d':$D},'petMapOfLists':{'a':[$P,$D]},'petListOfMaps':[{'a':$P}],"
                + "'petMapOfMaps':{'a':{'b':$D}},'petListOfLists':[[$P],[$D]],'catMap':{'c':$P},'vehicleMap':{'v':$V,'s':$VS},"
                + "'vehicleList':[$V,$VS],'carMap':{'c':{'kind':'CAR','doors':5}},'persianMap':{'p':$P},'stringMap':{'k':'v'},"
                + "'stringLists':[['a','b']]}";
        Map<String, String> results = runChecks(generate(CONTAINERS_SPEC, properties(legacy)), "ContainerPropertyChecks",
                "  static String q(String json) { return json.replace('\\'', '\"'); }",
                "  static final String P = q(\"{'petType':'PERSIAN','name':'tom','indoor':true,'longHair':true}\");",
                "  static final String D = q(\"{'petType':'DOG','name':'rex','barks':3}\");",
                "  static final String V = q(\"{'kind':'CAR','wheels':4,'doors':2}\");",
                "  static final String VS = q(\"{'kind':'VEHICLE','wheels':3}\");",
                "  static String json(String json) { return q(json).replace(\"$VS\", VS).replace(\"$P\", P).replace(\"$D\", D).replace(\"$V\", V); }",
                "  static final String ALL = json(\"" + allJson + "\");",
                "  static final java.lang.reflect.Type PET_MAP = new java.util.HashMap<String, Pet>() { }.getClass().getGenericSuperclass();",
                "  static final java.lang.reflect.Type VEHICLE_MAP = new java.util.HashMap<String, Vehicle>() { }.getClass().getGenericSuperclass();",
                "  static String d(Object o) {",
                "    if (o instanceof java.util.Map) {",
                "      java.util.List<String> entries = new java.util.ArrayList<>();",
                "      new java.util.TreeMap<>((java.util.Map<?, ?>) o).forEach((k, v) -> entries.add(k + \"=\" + d(v)));",
                "      return \"{\" + String.join(\",\", entries) + \"}\";",
                "    }",
                "    if (o instanceof java.util.Collection) {",
                "      java.util.List<String> elements = new java.util.ArrayList<>();",
                "      ((java.util.Collection<?>) o).forEach(e -> elements.add(d(e)));",
                "      return \"[\" + String.join(\",\", elements) + \"]\";",
                "    }",
                "    if (o instanceof PersianCat) { PersianCat c = (PersianCat) o; return \"PersianCat(\" + c.getName() + \",\" + c.getIndoor() + \",\" + c.getLongHair() + \")\"; }",
                "    if (o instanceof Dog) { Dog g = (Dog) o; return \"Dog(\" + g.getName() + \",\" + g.getBarks() + \",\" + d(g.getPack()) + \",\" + d(g.getFriends()) + \")\"; }",
                "    if (o instanceof Car) { Car c = (Car) o; return \"Car(\" + c.getWheels() + \",\" + c.getDoors() + \",\" + d(c.getTrailers()) + \")\"; }",
                "    if (o instanceof Vehicle) { Vehicle v = (Vehicle) o; return \"Vehicle(\" + v.getWheels() + \",\" + d(v.getTrailers()) + \")\"; }",
                "    if (o instanceof Shelter) { Shelter s = (Shelter) o; return \"Shelter(\" + s.getCity() + \",\" + d(s.getResidents()) + \")\"; }",
                "    if (o instanceof House) { return \"House(\" + d(((House) o).getResidents()) + \")\"; }",
                "    return String.valueOf(o);",
                "  }",
                "  static String all(Containers c) {",
                "    return \"petList=\" + d(c.getPetList()) + \" petSet=\" + d(c.getPetSet()) + \" petMap=\" + d(c.getPetMap())"
                        + " + \" petMapOfLists=\" + d(c.getPetMapOfLists()) + \" petListOfMaps=\" + d(c.getPetListOfMaps())"
                        + " + \" petMapOfMaps=\" + d(c.getPetMapOfMaps()) + \" petListOfLists=\" + d(c.getPetListOfLists())"
                        + " + \" catMap=\" + d(c.getCatMap()) + \" vehicleMap=\" + d(c.getVehicleMap()) + \" vehicleList=\" + d(c.getVehicleList())"
                        + " + \" carMap=\" + d(c.getCarMap()) + \" persianMap=\" + d(c.getPersianMap()) + \" stringMap=\" + d(c.getStringMap())"
                        + " + \" stringLists=\" + d(c.getStringLists());",
                "  }",
                "  /** the exception and whether it or one of its causes reports the given message */",
                "  static String failure(Jsonb j, String json, Class<?> type, String message) {",
                "    try {",
                "      return \"no exception: \" + d(j.fromJson(json(json), type));",
                "    } catch (jakarta.json.bind.JsonbException e) {",
                "      for (Throwable t = e; t != null; t = t.getCause()) {",
                "        if (String.valueOf(t.getMessage()).contains(message)) {",
                "          return \"JsonbException: \" + message;",
                "        }",
                "      }",
                "      return \"JsonbException without the message: \" + e.getMessage();",
                "    }",
                "  }",
                "  /** a failure of Johnzon before any container deserializer is called (see testContainerBinding), otherwise rethrown */",
                "  static String johnzonInternal(jakarta.json.bind.JsonbException e) {",
                "    Throwable root = e;",
                "    while (root.getCause() != null) {",
                "      root = root.getCause();",
                "    }",
                "    StackTraceElement[] stack = root.getStackTrace();",
                "    if (root instanceof NullPointerException && stack.length > 0 && stack[0].getClassName().startsWith(\"org.apache.johnzon.\")",
                "        && java.util.Arrays.stream(stack).noneMatch(frame -> frame.getClassName().contains(\"ContainerDeserializer\"))) {",
                "      return \"" + JOHNZON_MISSING_CREATOR_PARAMETER + "\";",
                "    }",
                "    throw e;",
                "  }",
                "  static void checks() throws Exception {",
                "    check(\"all\", j -> all(j.fromJson(ALL, Containers.class)));",
                "    check(\"set type\", j -> j.fromJson(ALL, Containers.class).getPetSet() instanceof java.util.Set);",
                "    check(\"repeated\", j -> { java.util.Set<String> r = new java.util.HashSet<>(); for (int i = 0; i < 3; i++) { r.add(all(j.fromJson(ALL, Containers.class))); } return r.size(); });",
                "    check(\"then roots\", j -> { j.fromJson(ALL, Containers.class); return d(j.fromJson(P, Pet.class)) + \" \" + d(j.fromJson(V, Vehicle.class)); });",
                "    check(\"after roots\", j -> { j.fromJson(P, Pet.class); j.fromJson(V, Vehicle.class); return all(j.fromJson(ALL, Containers.class)); });",
                "    check(\"after root containers\", j -> { for (java.lang.reflect.Type t : new java.lang.reflect.Type[] { PET_MAP, VEHICLE_MAP }) {"
                        + " try { j.fromJson(json(\"{'a':$P}\"), t); } catch (jakarta.json.bind.JsonbException e) { /* Yasson up to 3.0.4, see testYassonContainerLimitation */ } }"
                        + " return all(j.fromJson(ALL, Containers.class)); });",
                "    check(\"nulls\", j -> { Containers c = j.fromJson(json(\"{'petList':null,'petMap':{'a':null},'petListOfLists':[null,[null,$P]],'vehicleMap':null}\"), Containers.class);"
                        + " return d(c.getPetList()) + \" \" + d(c.getPetMap()) + \" \" + d(c.getPetListOfLists()) + \" \" + d(c.getVehicleMap()); });",
                "    check(\"missing required containers\", j -> { try { Required r = j.fromJson(json(\"{'note':'n'}\"), Required.class); return d(r.getPetMap()) + \" \" + d(r.getVehicleList()) + \" \" + r.getNote(); }"
                        + " catch (jakarta.json.bind.JsonbException e) { return johnzonInternal(e); } });",
                "    check(\"null required containers\", j -> { Required r = j.fromJson(json(\"{'petMap':null,'vehicleList':null}\"), Required.class); return d(r.getPetMap()) + \" \" + d(r.getVehicleList()); });",
                "    check(\"missing inherited required container\", j -> { try { return d(j.fromJson(json(\"{'houseType':'SHELTER','city':'x'}\"), House.class)); }"
                        + " catch (jakarta.json.bind.JsonbException e) { return johnzonInternal(e); } });",
                "    check(\"missing optional containers\", j -> all(j.fromJson(\"{}\", Containers.class)));",
                "    check(\"unknown in map\", j -> failure(j, \"{'petMap':{'a':{'petType':'FOX'}}}\", Containers.class, \"Unknown or missing discriminator value for 'petType' of Pet: FOX\"));",
                "    check(\"missing in list\", j -> failure(j, \"{'petList':[{'name':'x'}]}\", Containers.class, \"Unknown or missing discriminator value for 'petType' of Pet: null\"));",
                "    check(\"unknown in nested\", j -> failure(j, \"{'petMapOfLists':{'a':[{'petType':'FOX'}]}}\", Containers.class, \"Unknown or missing discriminator value for 'petType' of Pet: FOX\"));",
                "    check(\"Cat<-DOG in map\", j -> failure(j, \"{'catMap':{'a':$D}}\", Containers.class, \"Unknown or missing discriminator value for 'petType' of Cat: DOG\"));",
                "    check(\"array as map\", j -> failure(j, \"{'petMap':[$P]}\", Containers.class, \"\"));",
                "    check(\"creator\", j -> { Required r = j.fromJson(json(\"{'petMap':{'p':$P,'d':$D},'vehicleList':[$V,$VS],'note':'n'}\"), Required.class);"
                        + " return d(r.getPetMap()) + \" \" + d(r.getVehicleList()) + \" \" + r.getNote(); });",
                "    check(\"inherited creator\", j -> d(j.fromJson(json(\"{'houseType':'SHELTER','city':'x','residents':{'d':$D}}\"), House.class)));",
                "    check(\"transfer object\", j -> d(j.fromJson(json(\"{'houseType':'HOUSE','residents':{'p':$P}}\"), House.class)) + \" \""
                        + " + d(j.fromJson(json(\"{'kind':'VEHICLE','wheels':3,'trailers':[$V,$VS]}\"), Vehicle.class)));",
                "    check(\"subclass inherits\", j -> d(j.fromJson(json(\"{'kind':'CAR','doors':2,'trailers':[$V]}\"), Vehicle.class)));",
                "    check(\"subclass first\", j -> d(j.fromJson(json(\"{'petType':'DOG','name':'rex','pack':[$P,$D],'friends':{'p':$P}}\"), Dog.class)) + \" \""
                        + " + d(j.fromJson(json(\"{'houseType':'SHELTER','city':'x','residents':{'d':$D}}\"), Shelter.class)));",
                // reads the root Pet first, which Yasson up to 3.0.4 would need without the container deserializer ("subclass first" does not)
                "    check(\"polymorphic holder\", j -> d(j.fromJson(json(\"{'petType':'DOG','name':'rex','pack':[$P,$D],'friends':{'p':$P}}\"), Pet.class)));",
                "    check(\"roundtrip\", j -> { Containers c = j.fromJson(ALL, Containers.class); Required r = j.fromJson(json(\"{'petMap':{'p':$P},'vehicleList':[$V]}\"), Required.class);"
                        + " return c.equals(j.fromJson(j.toJson(c), Containers.class)) + \" \" + r.equals(j.fromJson(j.toJson(r), Required.class)); });",
                "  }");

        String p = "PersianCat(tom,true,true)";
        String d = "Dog(rex,3,[],{})";
        String v = "Car(4,2,[])";
        String vs = "Vehicle(3,[])";
        String all = "petList=[" + p + "," + d + "] petSet=[" + p + "] petMap={d=" + d + ",p=" + p + "} petMapOfLists={a=[" + p + "," + d + "]}"
                + " petListOfMaps=[{a=" + p + "}] petMapOfMaps={a={b=" + d + "}} petListOfLists=[[" + p + "],[" + d + "]] catMap={c=" + p + "}"
                + " vehicleMap={s=" + vs + ",v=" + v + "} vehicleList=[" + v + "," + vs + "] carMap={c=Car(null,5,[])} persianMap={p=" + p + "}"
                + " stringMap={k=v} stringLists=[[a,b]]";
        Map<String, String> expected = new LinkedHashMap<>();
        expected.put("all", all);
        expected.put("set type", "true");
        expected.put("repeated", "1");
        expected.put("then roots", p + " " + v);
        expected.put("after roots", all);
        expected.put("after root containers", all);
        expected.put("nulls", "null {a=null} [null,[null," + p + "]] null");
        String none = "petList=[] petSet=[] petMap={} petMapOfLists={} petListOfMaps=[] petMapOfMaps={} petListOfLists=[] catMap={}"
                + " vehicleMap={} vehicleList=[] carMap={} persianMap={} stringMap={} stringLists=[]";
        expected.put("missing required containers", "null null n");
        expectUnderJohnzon(expected, "missing required containers", JOHNZON_MISSING_CREATOR_PARAMETER);
        expected.put("null required containers", "null null");
        expected.put("missing inherited required container", "Shelter(x,null)");
        expectUnderJohnzon(expected, "missing inherited required container", JOHNZON_MISSING_CREATOR_PARAMETER);
        expected.put("missing optional containers", none);
        expected.put("unknown in map", "JsonbException: Unknown or missing discriminator value for 'petType' of Pet: FOX");
        expected.put("missing in list", "JsonbException: Unknown or missing discriminator value for 'petType' of Pet: null");
        expected.put("unknown in nested", "JsonbException: Unknown or missing discriminator value for 'petType' of Pet: FOX");
        expected.put("Cat<-DOG in map", "JsonbException: Unknown or missing discriminator value for 'petType' of Cat: DOG");
        expected.put("array as map", "JsonbException: ");
        expected.put("creator", "{d=" + d + ",p=" + p + "} [" + v + "," + vs + "] n");
        expected.put("inherited creator", "Shelter(x,{d=" + d + "})");
        expected.put("transfer object", "House({p=" + p + "}) Vehicle(3,[" + v + "," + vs + "])");
        expected.put("subclass inherits", "Car(null,2,[" + v + "])");
        expected.put("subclass first", "Dog(rex,null,[" + p + "," + d + "],{p=" + p + "}) Shelter(x,{d=" + d + "})");
        expected.put("polymorphic holder", "Dog(rex,null,[" + p + "," + d + "],{p=" + p + "})");
        expected.put("roundtrip", "true true");
        assertResults(results, expected);
    }

    @DataProvider
    public Object[][] oneOfCases() {
        return new Object[][]{
                // implicit mapping of oneOf members: only legacyDiscriminatorBehavior=false (see testImplicitOneOfMappingInLegacyMode)
                {"abstract_oneof_discriminator.yaml", false, "PersianCatRequest", "DogRequest"},
                {"abstract_oneof_discriminator_explicit.yaml", true, "PERSIAN", "DOG"},
                {"abstract_oneof_discriminator_explicit.yaml", false, "PERSIAN", "DOG"},
                // two abstract levels below the oneOf member
                {"abstract_oneof_deep_discriminator.yaml", false, "LION", "DogRequest"}};
    }

    /**
     * A sealed oneOf interface whose abstract member has concrete subclasses: they are read through the interface, the
     * abstract member and a list of the interface; the abstract member is not a discriminator value.
     */
    @Test(dataProvider = "oneOfCases")
    public void testOneOfInterfaceBinding(String spec, boolean legacy, String catValue, String dogValue) throws Exception {
        requireJdk(17);
        Map<String, Object> properties = properties(legacy);
        properties.put("useOneOfInterfaces", "true");
        Path output = generate(SPEC_DIR + spec, properties);
        assertFileContains(modelDir(output).resolve("PetRequest.java"),
                DESERIALIZER + "PetRequest.JsonbPolymorphicDeserializer.class)",
                "public sealed interface PetRequest permits CatRequest, DogRequest {");
        String leaf = "LION".equals(catValue) ? "LionRequest" : "PersianCatRequest";
        String cat = "\"{\\\"petType\\\":\\\"" + catValue + "\\\"}\"";
        String dog = "\"{\\\"petType\\\":\\\"" + dogValue + "\\\"}\"";
        Map<String, String> results = runChecks(output, "OneOfChecks",
                "  static void checks() throws Exception {",
                "    check(\"PetRequest<-cat\", j -> cls(j.fromJson(" + cat + ", PetRequest.class)));",
                "    check(\"PetRequest<-dog\", j -> cls(j.fromJson(" + dog + ", PetRequest.class)));",
                "    check(\"CatRequest<-cat\", j -> cls(j.fromJson(" + cat + ", CatRequest.class)));",
                "    check(\"PetRequest<-CatRequest\", j -> cls(j.fromJson(\"{\\\"petType\\\":\\\"CatRequest\\\"}\", PetRequest.class)));",
                // see testYassonContainerLimitation (Yasson 3.0.2 only)
                "    check(\"List<PetRequest>\", j -> { readTypesFirst(j, PetRequest.class); java.lang.reflect.Type t = new java.util.ArrayList<PetRequest>() { }.getClass().getGenericSuperclass();"
                        + " java.util.List<PetRequest> l = j.fromJson(\"[\" + " + cat + " + \",\" + " + dog + " + \"]\", t);"
                        + " return cls(l.get(0)) + \",\" + cls(l.get(1)) + \" \" + j.toJson(l).equals(j.toJson(j.fromJson(j.toJson(l), t))); });",
                "    check(\"constructors\", j -> cls(j.fromJson(j.toJson(new " + leaf + "()), PetRequest.class)) + \",\" + cls(j.fromJson(j.toJson(new DogRequest()), PetRequest.class))"
                        + " + \" \" + new " + leaf + "().getPetType() + \",\" + new DogRequest().getPetType());",
                "  }");
        Map<String, String> expected = new LinkedHashMap<>();
        expected.put("PetRequest<-cat", leaf);
        expected.put("PetRequest<-dog", "DogRequest");
        expected.put("CatRequest<-cat", leaf);
        expected.put("PetRequest<-CatRequest", "JsonbException: Unknown or missing discriminator value for 'petType' of PetRequest: CatRequest");
        expected.put("List<PetRequest>", leaf + ",DogRequest true");
        expected.put("constructors", leaf + ",DogRequest " + catValue + "," + dogValue);
        assertResults(results, expected);
    }

    /**
     * Existing limitation shared with Jackson: with {@code legacyDiscriminatorBehavior=true} a oneOf interface without
     * explicit mapping maps no members, so it gets no deserializer; its abstract member still gets one.
     */
    @Test
    public void testImplicitOneOfMappingInLegacyMode() throws Exception {
        Map<String, Object> properties = properties(true);
        properties.put("useOneOfInterfaces", "true");
        Path modelDir = modelDir(generate(SPEC_DIR + "abstract_oneof_discriminator.yaml", properties));
        assertFileNotContains(modelDir.resolve("PetRequest.java"), "JsonbTypeDeserializer(");
        assertFileContains(modelDir.resolve("CatRequest.java"), DESERIALIZER + "CatRequest.JsonbPolymorphicDeserializer.class)");
    }

    /**
     * The discriminator values that concrete models are created with: inline and referenced enums, ambiguous mappings
     * (no default, also not the one of a concrete parent), a concrete root that no value maps to itself (its type name)
     * and a second discriminator level. The models have no JSON-B creator, so JSON-B also creates them with the
     * no-argument constructor: a discriminator in the JSON overrides the default. A deserializer applied to a subclass
     * of its model (as Johnzon does) rejects an object of another subclass instead of returning it.
     */
    @Test(dataProvider = "legacyDiscriminatorBehavior")
    public void testDiscriminatorDefaults(boolean legacy) throws Exception {
        requireJdk(17);
        Path output = generate(DEFAULTS_SPEC, properties(legacy));
        Path modelDir = modelDir(output);
        assertFileContains(modelDir.resolve("Circle.java"), "public Circle() { this.setShapeType(" + MODEL_PACKAGE + ".Shape.ShapeTypeEnum.CIRCLE); }",
                "this.shapeType(" + MODEL_PACKAGE + ".Shape.ShapeTypeEnum.CIRCLE);");
        assertFileContains(modelDir.resolve("Bird.java"), "public Bird() { this.setKind(" + MODEL_PACKAGE + ".AnimalKind.BIRD); }");
        // APPLE and MALUS both map to Apple
        assertFileContains(modelDir.resolve("Apple.java"), "public Apple() { }");
        assertFileNotContains(modelDir.resolve("Apple.java"), "this.setFruitType(", "this.fruitType(\"");
        assertFileContains(modelDir.resolve("ArmRobot.java"), "this.setMachineType(\"ROBOT\");", "this.setRobotType(\"ARM\");");
        // DRONE and UAV both map to Drone: it must not keep the default of its concrete parent Machine
        assertFileContains(modelDir.resolve("Drone.java"), "public Drone() { this.setMachineType(null); }", "this.machineType(null);");

        Map<String, String> results = runChecks(output, "DefaultChecks",
                "  static void checks() throws Exception {",
                "    check(\"Circle\", j -> { Circle c = Circle.builder().radius(1).build(); Shape s = j.fromJson(j.toJson(c), Shape.class); return new Circle().getShapeType() + \" \" + c.getShapeType() + \" \" + cls(s) + \" \" + c.equals(s); });",
                "    check(\"Bird\", j -> { Bird b = new Bird(); b.setWings(2); Animal a = j.fromJson(j.toJson(b), Animal.class); return b.getKind() + \" \" + Bird.builder().build().getKind() + \" \" + cls(a) + \" \" + b.equals(a); });",
                "    check(\"Apple, Pear\", j -> new Apple().getFruitType() + \" \" + Apple.builder().build().getFruitType() + \" \" + new Pear().getFruitType() + \" \" + Pear.builder().build().getFruitType());",
                "    check(\"Machine\", j -> { Machine m = Machine.builder().name(\"m\").build(); Machine r = j.fromJson(j.toJson(m), Machine.class); return m.getMachineType() + \" \" + cls(r) + \" \" + m.equals(r); });",
                "    check(\"Robot\", j -> { Robot m = new Robot(); m.setArms(2); Machine r = j.fromJson(j.toJson(m), Machine.class); return m.getMachineType() + \"/\" + m.getRobotType() + \" \" + cls(r) + \" \" + m.equals(r); });",
                "    check(\"ArmRobot\", j -> { ArmRobot m = ArmRobot.builder().reach(3).build(); Machine r = j.fromJson(j.toJson(m), Machine.class); return m.getMachineType() + \"/\" + m.getRobotType() + \" \" + cls(r) + \" \" + m.equals(r); });",
                "    check(\"Drone\", j -> new Drone().getMachineType() + \" \" + Drone.builder().build().getMachineType() + \" \" + cls(j.fromJson(\"{\\\"machineType\\\":\\\"UAV\\\"}\", Machine.class)));",
                "    check(\"JSON overrides default\", j -> j.fromJson(\"{\\\"shapeType\\\":\\\"SQUARE\\\"}\", Circle.class).getShapeType());",
                "    check(\"JSON without discriminator\", j -> j.fromJson(\"{\\\"radius\\\":1}\", Circle.class).getShapeType());",
                "    check(\"parent deserializer for SQUARE as Circle\", j -> cls(j.fromJson(\"{\\\"shapeType\\\":\\\"SQUARE\\\"}\", (java.lang.reflect.Type) AsCircle.class)));",
                "    check(\"parent deserializer for CIRCLE as Circle\", j -> cls(j.fromJson(\"{\\\"shapeType\\\":\\\"CIRCLE\\\"}\", (java.lang.reflect.Type) AsCircle.class)));",
                "    check(\"second discriminator missing\", j -> { Robot r = (Robot) j.fromJson(\"{\\\"machineType\\\":\\\"ROBOT\\\",\\\"arms\\\":2}\", Machine.class); return r.getRobotType() + \" \" + r.getArms(); });",
                "  }",
                // calls the deserializer of Shape for the requested subclass Circle, as Johnzon does
                "  public static final class AsCircleDeserializer implements jakarta.json.bind.serializer.JsonbDeserializer<Object> {",
                "    public Object deserialize(jakarta.json.stream.JsonParser parser, jakarta.json.bind.serializer.DeserializationContext ctx, java.lang.reflect.Type type) {",
                "      return new Shape.JsonbPolymorphicDeserializer().deserialize(parser, ctx, Circle.class);",
                "    }",
                "  }",
                "  @jakarta.json.bind.annotation.JsonbTypeDeserializer(AsCircleDeserializer.class)",
                "  public static final class AsCircle {",
                "  }");
        Map<String, String> expected = new LinkedHashMap<>();
        expected.put("Circle", "CIRCLE CIRCLE Circle true");
        expected.put("Bird", "BIRD BIRD Bird true");
        expected.put("Apple, Pear", "null null PEAR PEAR");
        expected.put("Machine", "Machine Machine true");
        expected.put("Robot", "ROBOT/ROBOT Robot true");
        expected.put("ArmRobot", "ROBOT/ARM ArmRobot true");
        expected.put("Drone", "null null Drone");
        expected.put("JSON overrides default", "SQUARE");
        // the only change for existing JSON: a model created by its no-argument constructor keeps its default
        expected.put("JSON without discriminator", "CIRCLE");
        expected.put("parent deserializer for SQUARE as Circle", "JsonbException: The discriminator of the JSON object resolves to "
                + MODEL_PACKAGE + ".Square, which is not a " + MODEL_PACKAGE + ".Circle");
        expected.put("parent deserializer for CIRCLE as Circle", "Circle");
        expected.put("second discriminator missing", "null 2");
        // Johnzon reads a Circle with the deserializer of Shape, which resolves the class from the JSON
        expectUnderJohnzon(expected, "JSON overrides default", "JsonbException: The discriminator of the JSON object resolves to "
                + MODEL_PACKAGE + ".Square, which is not a " + MODEL_PACKAGE + ".Circle");
        expectUnderJohnzon(expected, "JSON without discriminator", "JsonbException: Unknown or missing discriminator value for 'shapeType' of Shape: null");
        assertResults(results, expected);
    }

    /**
     * A discriminator property whose Java type is not {@code String} (here a string of format uri, a {@code URI}) gets
     * no default, and the models still compile.
     */
    @Test
    public void testDiscriminatorDefaultsOnlyForStringsAndEnums() throws Exception {
        Path output = generate("src/test/resources/3_0/oneOfDiscriminator.yaml", properties(true));
        compileModels(output, classpath(Jsonb.class, JsonParser.class));
        assertFileContains(modelDir(output).resolve("PetUriDisc.java"), "public PetUriDisc() { }", "private URI petType;");
        assertFileContains(modelDir(output).resolve("CatUriDisc.java"), "public CatUriDisc() { }");
    }

    /**
     * Reads the same JSON with Jackson and with JSON-B. Both resolve a concrete root that no value maps to itself under
     * its type name, and a concrete intermediate type without a value of its own (second) discriminator as itself. They
     * differ where Jackson does not dispatch on the second discriminator or accepts the value of a subtype's
     * discriminator: the JSON-B deserializer follows the mappings of the specification.
     */
    @Test(dataProvider = "legacyDiscriminatorBehavior")
    public void testConcreteTypeResolutionComparedWithJackson(boolean legacy) throws Exception {
        Map<String, String> same = new LinkedHashMap<>();
        same.put("Machine<-{\"machineType\":\"Machine\"}", "Machine");
        same.put("Machine<-{\"machineType\":\"ROBOT\"}", "Robot");
        same.put("Machine<-{\"machineType\":\"ROBOT\",\"robotType\":\"ROBOT\"}", "Robot");
        same.put("Machine<-{\"machineType\":\"XX\"}", "error");
        same.put("Machine<-{}", "error");
        same.put("Robot<-{\"robotType\":\"ROBOT\"}", "Robot");
        same.put("Robot<-{\"robotType\":\"ARM\"}", "ArmRobot");
        same.put("Robot<-{}", "error");
        // the expected results of Jackson and of JSON-B
        Map<String, List<String>> differences = new LinkedHashMap<>();
        differences.put("Machine<-{\"machineType\":\"ROBOT\",\"robotType\":\"ARM\"}", Arrays.asList("Robot", "ArmRobot"));
        differences.put("Machine<-{\"machineType\":\"ROBOT\",\"robotType\":\"XX\"}", Arrays.asList("Robot", "error"));
        differences.put("Machine<-{\"machineType\":\"ARM\"}", Arrays.asList("ArmRobot", "error"));

        Map<String, Object> jacksonProperties = properties(legacy);
        jacksonProperties.remove(CodegenConstants.SERIALIZATION_LIBRARY);
        jacksonProperties.remove(JavaJAXRSSpecServerCodegen.JSONB_DISCRIMINATOR_DESERIALIZERS);
        jacksonProperties.put("openApiNullable", "false");
        Path jacksonClasses = compileModels(generate(DEFAULTS_SPEC, jacksonProperties), classpath(JsonProperty.class));
        Path jsonbClasses = compileModels(generate(DEFAULTS_SPEC, properties(legacy)), classpath(Jsonb.class, JsonParser.class));
        Map<String, String> jackson = new LinkedHashMap<>();
        Map<String, String> jsonb = new LinkedHashMap<>();
        Map<String, String> expectedJackson = new LinkedHashMap<>(same);
        Map<String, String> expectedJsonb = new LinkedHashMap<>(same);
        differences.forEach((key, results) -> {
            expectedJackson.put(key, results.get(0));
            expectedJsonb.put(key, results.get(1));
        });
        ObjectMapper mapper = new ObjectMapper();
        try (URLClassLoader jacksonLoader = loader(jacksonClasses); URLClassLoader jsonbLoader = loader(jsonbClasses);
             Jsonb binding = JsonbBuilder.create()) {
            for (String key : expectedJackson.keySet()) {
                String type = MODEL_PACKAGE + "." + key.substring(0, key.indexOf("<-"));
                String json = key.substring(key.indexOf("<-") + 2);
                jackson.put(key, read(() -> mapper.readValue(json, jacksonLoader.loadClass(type))));
                jsonb.put(key, read(() -> binding.fromJson(json, jsonbLoader.loadClass(type))));
            }
        }
        assertEquals(jackson, expectedJackson);
        assertEquals(jsonb, expectedJsonb);
    }

    /**
     * Models named like the helper classes nested in a polymorphic model: the helper classes get other names, so that
     * they do not shadow the models in the polymorphic model and its subclasses, which therefore keep their types.
     */
    @Test
    public void testHelperClassesDoNotShadowModels() throws Exception {
        requireJdk(17);
        Path output = generate(SPEC_DIR + "jsonb_polymorphism_name_clash.yaml", properties(false));
        assertFileContains(modelDir(output).resolve("Document.java"), "private JsonbData data;", "private JsonbPolymorphicDeserializer helper;",
                "private JsonbPolymorphicContainerDeserializer container;", "class JsonbPolymorphicContainerDeserializer2 implements",
                DESERIALIZER + "Document.JsonbPolymorphicContainerDeserializer2.class)");
        assertFileNotContains(modelDir(output).resolve("Document.java"), "class JsonbData {", "class JsonbData2 {",
                "class JsonbPolymorphicDeserializer implements", "class JsonbPolymorphicContainerDeserializer implements");
        // the checks only compile if the properties have the types of the models
        Map<String, String> results = runChecks(output, "NameChecks",
                "  static void checks() throws Exception {",
                "    check(\"Document\", j -> { JsonbData data = new JsonbData(); data.setValue(\"v\"); JsonbPolymorphicDeserializer helper = new JsonbPolymorphicDeserializer(); helper.setId(7);"
                        + " Document d = Document.builder().data(data).helper(helper).build(); Document r = j.fromJson(j.toJson(d), Document.class);"
                        + " JsonbData readData = r.getData(); JsonbPolymorphicDeserializer readHelper = r.getHelper(); return cls(r) + \" \" + readData.getValue() + \" \" + readHelper.getId() + \" \" + d.equals(r); });",
                "    check(\"Report\", j -> { JsonbData summary = new JsonbData(); summary.setValue(\"s\"); JsonbData2 page = new JsonbData2(); page.setNumber(1);"
                        + " Report report = new Report(); report.setSummary(summary); report.addPagesItem(page); Document r = j.fromJson(j.toJson(report), Document.class);"
                        + " JsonbData readSummary = ((Report) r).getSummary(); JsonbData2 readPage = ((Report) r).getPages().get(0); return cls(r) + \" \" + readSummary.getValue() + \" \" + readPage.getNumber() + \" \" + report.equals(r); });",
                "    check(\"related\", j -> { JsonbPolymorphicContainerDeserializer container = new JsonbPolymorphicContainerDeserializer(); container.setLabel(\"l\");"
                        + " Document d = Document.builder().container(container).related(java.util.Arrays.asList(new Report(), new Document())).build(); Document r = j.fromJson(j.toJson(d), Document.class);"
                        + " JsonbPolymorphicContainerDeserializer readContainer = r.getContainer(); return readContainer.getLabel() + \" \" + cls(r.getRelated().get(0)) + \",\" + cls(r.getRelated().get(1)) + \" \" + d.equals(r); });",
                "  }");
        Map<String, String> expected = new LinkedHashMap<>();
        expected.put("Document", "Document v 7 true");
        expected.put("Report", "Report s 1 true");
        expected.put("related", "l Report,Document true");
        assertResults(results, expected);
    }

    /**
     * The deserializers look up the JSON-P provider once, not once per object: a provider that counts its instances is
     * registered for the run (and the JSON-B implementation gets its own provider).
     */
    @Test
    public void testJsonProviderIsLookedUpOncePerDeserializer() throws Exception {
        requireJdk(17);
        String sportsCar = "{\\\"kind\\\":\\\"CAR\\\",\\\"carType\\\":\\\"SPORTS\\\",\\\"horsePower\\\":1}";
        Map<String, String> results = runWithCountingProvider(generate(SPEC, properties(false)), "ProviderChecks",
                "  public static java.util.Map<String, String> run() throws Exception {",
                "    java.util.Map<String, String> results = new java.util.LinkedHashMap<>();",
                "    try (jakarta.json.bind.Jsonb j = jakarta.json.bind.JsonbBuilder.newBuilder().withProvider(CountingJsonProvider.delegate()).build()) {",
                "      int before = CountingJsonProvider.INSTANCES.get();",
                "      Vehicle first = j.fromJson(\"" + sportsCar + "\", Vehicle.class);",
                "      int warm = CountingJsonProvider.INSTANCES.get();",
                "      java.util.StringJoiner json = new java.util.StringJoiner(\",\", \"[\", \"]\");",
                "      for (int i = 0; i < 100; i++) {",
                "        json.add(\"" + sportsCar + "\");",
                "      }",
                "      java.lang.reflect.Type type = new java.util.ArrayList<Vehicle>() { }.getClass().getGenericSuperclass();",
                "      java.util.List<Vehicle> vehicles = j.fromJson(json.toString(), type);",
                "      results.put(\"first object\", String.valueOf(warm - before));",
                "      results.put(\"100 more objects\", String.valueOf(CountingJsonProvider.INSTANCES.get() - warm));",
                "      results.put(\"read\", first.getClass().getSimpleName() + \" \" + vehicles.size() + \" \" + vehicles.stream().allMatch(SportsCar.class::isInstance));",
                "    }",
                "    return results;",
                "  }");
        Map<String, String> expected = new LinkedHashMap<>();
        // once by the deserializer of Vehicle and once by that of Car, which it delegates to
        expected.put("first object", "2");
        expected.put("100 more objects", "0");
        expected.put("read", "SportsCar 100 true");
        assertResults(results, expected);
    }

    /**
     * The deserializers close the parsers they create to bind a subtype or a model through its transfer object, also
     * when the binding fails: the counting provider counts the parsers it creates and closes.
     */
    @Test
    public void testParsersAreClosed() throws Exception {
        requireJdk(17);
        Map<String, String> results = runWithCountingProvider(generate(SPEC, properties(false)), "CloseChecks",
                "  static String check(jakarta.json.bind.Jsonb j, String json, java.lang.reflect.Type type) {",
                "    int opened = CountingJsonProvider.OPENED.get();",
                "    int closed = CountingJsonProvider.CLOSED.get();",
                "    String result;",
                "    try {",
                "      result = j.fromJson(json.replace('\\'', '\"'), type).getClass().getSimpleName();",
                "    } catch (jakarta.json.bind.JsonbException e) {",
                "      result = \"JsonbException\";",
                "    }",
                "    int newlyOpened = CountingJsonProvider.OPENED.get() - opened;",
                "    return result + \", parsers: \" + (newlyOpened > 0 ? \"created\" : \"none\") + \", not closed: \" + (newlyOpened - (CountingJsonProvider.CLOSED.get() - closed));",
                "  }",
                "  public static java.util.Map<String, String> run() throws Exception {",
                "    java.util.Map<String, String> results = new java.util.LinkedHashMap<>();",
                "    try (jakarta.json.bind.Jsonb j = jakarta.json.bind.JsonbBuilder.newBuilder().withProvider(CountingJsonProvider.delegate()).build()) {",
                "      results.put(\"subtype\", check(j, \"{'kind':'CAR','carType':'SPORTS','horsePower':1}\", Vehicle.class));",
                "      results.put(\"subtype fails\", check(j, \"{'kind':'CAR','carType':'SPORTS','horsePower':'x'}\", Vehicle.class));",
                "      results.put(\"transfer object\", check(j, \"{'kind':'VEHICLE','wheels':3}\", Vehicle.class));",
                "      results.put(\"transfer object fails\", check(j, \"{'kind':'VEHICLE','wheels':'x'}\", Vehicle.class));",
                "      results.put(\"containers\", check(j, \"{'pets':[{'petType':'PERSIAN','name':'a'}],'petsByName':{'b':{'petType':'SIAMESE','name':'b'}}}\", Holder.class));",
                "      results.put(\"container element fails\", check(j, \"{'vehicles':[{'kind':'CAR','carType':'SPORTS','horsePower':'x'}]}\", Holder.class));",
                "    }",
                "    return results;",
                "  }");
        Map<String, String> expected = new LinkedHashMap<>();
        expected.put("subtype", "SportsCar, parsers: created, not closed: 0");
        expected.put("subtype fails", "JsonbException, parsers: created, not closed: 0");
        expected.put("transfer object", "Vehicle, parsers: created, not closed: 0");
        expected.put("transfer object fails", "JsonbException, parsers: created, not closed: 0");
        expected.put("containers", "Holder, parsers: created, not closed: 0");
        expected.put("container element fails", "JsonbException, parsers: created, not closed: 0");
        assertResults(results, expected);
    }

    /**
     * Compiles the models and a checks class with the given body (a static {@code run()} method returning the results),
     * and runs it with a JSON-P provider that the deserializers look up and that counts its instances and the parsers it
     * creates and closes; it delegates to the provider of the test classpath (Parsson, or that of Johnzon), and the JSON-B
     * implementation is given its own instance of that, so that only the deserializers use the counting one.
     */
    private Map<String, String> runWithCountingProvider(Path output, String className, String... body) throws Exception {
        String classpath = classpath(Jsonb.class, JsonParser.class);
        Path classes = compileModels(output, classpath);
        Path sources = writeSource(output.resolve("checks"), "CountingJsonProvider",
                "package " + MODEL_PACKAGE + ";",
                "import jakarta.json.spi.JsonProvider;",
                "import jakarta.json.stream.JsonParser;",
                "import java.util.Map;",
                "import java.util.concurrent.atomic.AtomicInteger;",
                "public final class CountingJsonProvider extends JsonProvider {",
                "  public static final AtomicInteger INSTANCES = new AtomicInteger();",
                "  public static final AtomicInteger OPENED = new AtomicInteger();",
                "  public static final AtomicInteger CLOSED = new AtomicInteger();",
                "  private final JsonProvider delegate;",
                "  public CountingJsonProvider() throws ReflectiveOperationException {",
                "    INSTANCES.incrementAndGet();",
                "    delegate = delegate();",
                "  }",
                "  /** a new instance of the provider of the test classpath (such as that of Parsson or Johnzon) */",
                "  static JsonProvider delegate() throws ReflectiveOperationException {",
                "    for (JsonProvider provider : java.util.ServiceLoader.load(JsonProvider.class, CountingJsonProvider.class.getClassLoader().getParent())) {",
                "      if (!(provider instanceof CountingJsonProvider)) {",
                "        return provider.getClass().getDeclaredConstructor().newInstance();",
                "      }",
                "    }",
                "    throw new IllegalStateException(\"no JSON-P provider on the classpath\");",
                "  }",
                "  public JsonParser createParser(java.io.Reader r) { return counted(delegate.createParser(r)); }",
                "  public JsonParser createParser(java.io.InputStream in) { return counted(delegate.createParser(in)); }",
                "  static JsonParser counted(JsonParser parser) {",
                "    OPENED.incrementAndGet();",
                "    return new JsonParser() {",
                "      private boolean closed;",
                "      public boolean hasNext() { return parser.hasNext(); }",
                "      public Event next() { return parser.next(); }",
                "      public Event currentEvent() { return parser.currentEvent(); }",
                "      public String getString() { return parser.getString(); }",
                "      public boolean isIntegralNumber() { return parser.isIntegralNumber(); }",
                "      public int getInt() { return parser.getInt(); }",
                "      public long getLong() { return parser.getLong(); }",
                "      public java.math.BigDecimal getBigDecimal() { return parser.getBigDecimal(); }",
                "      public jakarta.json.stream.JsonLocation getLocation() { return parser.getLocation(); }",
                "      public jakarta.json.JsonObject getObject() { return parser.getObject(); }",
                "      public jakarta.json.JsonValue getValue() { return parser.getValue(); }",
                "      public jakarta.json.JsonArray getArray() { return parser.getArray(); }",
                "      public java.util.stream.Stream<jakarta.json.JsonValue> getArrayStream() { return parser.getArrayStream(); }",
                "      public java.util.stream.Stream<Map.Entry<String, jakarta.json.JsonValue>> getObjectStream() { return parser.getObjectStream(); }",
                "      public java.util.stream.Stream<jakarta.json.JsonValue> getValueStream() { return parser.getValueStream(); }",
                "      public void skipArray() { parser.skipArray(); }",
                "      public void skipObject() { parser.skipObject(); }",
                "      public void close() { if (!closed) { closed = true; CLOSED.incrementAndGet(); } parser.close(); }",
                "    };",
                "  }",
                "  public jakarta.json.stream.JsonParserFactory createParserFactory(Map<String, ?> c) { return delegate.createParserFactory(c); }",
                "  public jakarta.json.stream.JsonGenerator createGenerator(java.io.Writer w) { return delegate.createGenerator(w); }",
                "  public jakarta.json.stream.JsonGenerator createGenerator(java.io.OutputStream out) { return delegate.createGenerator(out); }",
                "  public jakarta.json.stream.JsonGeneratorFactory createGeneratorFactory(Map<String, ?> c) { return delegate.createGeneratorFactory(c); }",
                "  public jakarta.json.JsonReader createReader(java.io.Reader r) { return delegate.createReader(r); }",
                "  public jakarta.json.JsonReader createReader(java.io.InputStream in) { return delegate.createReader(in); }",
                "  public jakarta.json.JsonWriter createWriter(java.io.Writer w) { return delegate.createWriter(w); }",
                "  public jakarta.json.JsonWriter createWriter(java.io.OutputStream out) { return delegate.createWriter(out); }",
                "  public jakarta.json.JsonWriterFactory createWriterFactory(Map<String, ?> c) { return delegate.createWriterFactory(c); }",
                "  public jakarta.json.JsonReaderFactory createReaderFactory(Map<String, ?> c) { return delegate.createReaderFactory(c); }",
                "  public jakarta.json.JsonObjectBuilder createObjectBuilder() { return delegate.createObjectBuilder(); }",
                "  public jakarta.json.JsonArrayBuilder createArrayBuilder() { return delegate.createArrayBuilder(); }",
                "  public jakarta.json.JsonBuilderFactory createBuilderFactory(Map<String, ?> c) { return delegate.createBuilderFactory(c); }",
                "}");
        List<String> lines = new ArrayList<>(Arrays.asList("package " + MODEL_PACKAGE + ";", "public final class " + className + " {"));
        lines.addAll(Arrays.asList(body));
        lines.add("}");
        writeSource(sources, className, lines.toArray(new String[0]));
        assertCompiles(sources, classes, "17", classpath);
        Path services = Files.createDirectories(classes.resolve("META-INF/services"));
        Files.writeString(services.resolve("jakarta.json.spi.JsonProvider"), MODEL_PACKAGE + ".CountingJsonProvider\n");

        Thread thread = Thread.currentThread();
        ClassLoader contextClassLoader = thread.getContextClassLoader();
        try (URLClassLoader loader = new URLClassLoader(new URL[]{classes.toUri().toURL()}, getClass().getClassLoader()) {
            @Override
            public Enumeration<URL> getResources(String name) throws IOException {
                // the counting provider comes before the one of the test classpath
                return name.equals("META-INF/services/jakarta.json.spi.JsonProvider") ? findResources(name) : super.getResources(name);
            }
        }) {
            thread.setContextClassLoader(loader);
            @SuppressWarnings("unchecked")
            Map<String, String> results = (Map<String, String>) loader.loadClass(MODEL_PACKAGE + "." + className).getMethod("run").invoke(null);
            return new LinkedHashMap<>(results);
        } finally {
            thread.setContextClassLoader(contextClassLoader);
        }
    }

    /**
     * Known Yasson limitation (documented at {@code jsonbDiscriminatorDeserializers}): when a fresh Jsonb reads a root container of a
     * polymorphic type before the type itself, Yasson binds the values without the type's deserializer and caches that
     * binding for the type. For the abstract Pet this fails, for the concrete Vehicle it silently yields the wrong class.
     * Yasson 3.0.3 and 3.0.4 are affected for maps, 3.0.2 also for lists; 3.0.5 is not. Container properties of the
     * generated models are not affected (see testContainerBinding).
     * <p>
     * Reading the type first works around it; that is asserted. The limitation itself is classified: correct results
     * (Johnzon, a fixed Yasson) pass, exactly the known wrong results skip the test with a description of them, so that
     * it stays visible in the report without failing the build; any other result fails. To run against Yasson 3.0.2:
     * {@code -Dmaven.test.dependency.excludes=org.eclipse:yasson -Dmaven.test.additionalClasspath=<yasson-3.0.2.jar>}.
     */
    @Test
    public void testYassonContainerLimitation() throws Exception {
        requireJdk(17);
        String pets = "{\\\"a\\\":{\\\"petType\\\":\\\"PERSIAN\\\",\\\"name\\\":\\\"tom\\\"}}";
        String vehicles = "{\\\"a\\\":{\\\"kind\\\":\\\"CAR\\\",\\\"carType\\\":\\\"SPORTS\\\"}}";
        String petList = "[{\\\"petType\\\":\\\"PERSIAN\\\",\\\"name\\\":\\\"tom\\\"}]";
        String pet = "{\\\"petType\\\":\\\"PERSIAN\\\"}";
        String vehicle = "{\\\"kind\\\":\\\"CAR\\\",\\\"carType\\\":\\\"SPORTS\\\"}";
        Map<String, String> results = runChecks(generate(SPEC, properties(false)), "ContainerChecks",
                "  static final java.lang.reflect.Type PETS = new java.util.HashMap<String, Pet>() { }.getClass().getGenericSuperclass();",
                "  static final java.lang.reflect.Type VEHICLES = new java.util.HashMap<String, Vehicle>() { }.getClass().getGenericSuperclass();",
                "  static final java.lang.reflect.Type PET_LIST = new java.util.ArrayList<Pet>() { }.getClass().getGenericSuperclass();",
                "  static String read(Jsonb j, String json, java.lang.reflect.Type type) {",
                "    try {",
                "      Object value = j.fromJson(json, type);",
                "      return cls(value instanceof java.util.Map ? ((java.util.Map<?, ?>) value).get(\"a\")",
                "          : value instanceof java.util.List ? ((java.util.List<?>) value).get(0) : value);",
                "    } catch (jakarta.json.bind.JsonbException e) {",
                "      return \"JsonbException\";",
                "    }",
                "  }",
                "  static void checks() throws Exception {",
                "    check(\"Map<String,Pet> first\", j -> read(j, \"" + pets + "\", PETS) + \", then Pet: \" + read(j, \"" + pet + "\", Pet.class));",
                "    check(\"Map<String,Vehicle> first\", j -> read(j, \"" + vehicles + "\", VEHICLES) + \", then Vehicle: \" + read(j, \"" + vehicle + "\", Vehicle.class));",
                "    check(\"List<Pet> first\", j -> read(j, \"" + petList + "\", PET_LIST) + \", then Pet: \" + read(j, \"" + pet + "\", Pet.class));",
                "    check(\"Map<String,Pet> after Pet\", j -> { readTypesFirst(j, Pet.class); return read(j, \"" + pets + "\", PETS); });",
                "    check(\"Map<String,Vehicle> after Vehicle\", j -> { readTypesFirst(j, Vehicle.class); return read(j, \"" + vehicles + "\", VEHICLES); });",
                "    check(\"List<Pet> after Pet\", j -> { readTypesFirst(j, Pet.class); return read(j, \"" + petList + "\", PET_LIST); });",
                "  }");

        // the workaround
        assertEquals(results.remove("Map<String,Pet> after Pet"), "PersianCat");
        assertEquals(results.remove("Map<String,Vehicle> after Vehicle"), "SportsCar");
        assertEquals(results.remove("List<Pet> after Pet"), "PersianCat");

        Map<String, String> correct = new LinkedHashMap<>();
        correct.put("Map<String,Pet> first", "PersianCat, then Pet: PersianCat");
        correct.put("Map<String,Vehicle> first", "SportsCar, then Vehicle: SportsCar");
        correct.put("List<Pet> first", "PersianCat, then Pet: PersianCat");
        Map<String, String> knownYasson = new LinkedHashMap<>();
        // abstract type: the values cannot be created, and neither can the type afterwards
        knownYasson.put("Map<String,Pet> first", "JsonbException, then Pet: JsonbException");
        // concrete type: the values are silently created as the type itself, and so is the type afterwards
        knownYasson.put("Map<String,Vehicle> first", "Vehicle, then Vehicle: Vehicle");
        knownYasson.put("List<Pet> first", "JsonbException, then Pet: JsonbException");

        List<String> reproduced = new ArrayList<>();
        results.forEach((check, result) -> {
            if (result.equals(knownYasson.get(check))) {
                reproduced.add(check + " -> " + result);
            } else {
                assertEquals(result, correct.get(check), check);
            }
        });
        if (!reproduced.isEmpty()) {
            throw new SkipException("Known Yasson limitation reproduced (correct would be " + correct + "): " + reproduced);
        }
    }

    private static String read(Callable<Object> read) {
        try {
            return read.call().getClass().getSimpleName();
        } catch (Exception e) {
            return "error";
        }
    }

    /**
     * Compiles the models and a checks class with the given body (a static {@code checks()} method calling
     * {@code check(name, j -> ...)}), runs it with the JSON-B implementation of the test classpath and returns the
     * results by check name; an exception is reported as "SimpleName: first message line". Every check gets a new Jsonb
     * instance, so that a check does not depend on the types that other checks have read.
     */
    private static Map<String, String> runChecks(Path output, String className, String... body) throws Exception {
        String classpath = classpath(Jsonb.class, JsonParser.class);
        Path classes = compileModels(output, classpath);
        List<String> lines = new ArrayList<>(Arrays.asList(
                "package " + MODEL_PACKAGE + ";",
                "import jakarta.json.bind.Jsonb;",
                "public final class " + className + " {",
                "  static final java.util.Map<String, String> RESULTS = new java.util.LinkedHashMap<>();",
                "  static String cls(Object o) { return o == null ? \"null\" : o.getClass().getSimpleName(); }",
                "  interface Check { Object run(Jsonb j) throws Exception; }",
                "  static void check(String name, Check check) {",
                "    String result;",
                "    try (Jsonb j = jakarta.json.bind.JsonbBuilder.create()) {",
                "      result = String.valueOf(check.run(j));",
                "    } catch (Exception e) {",
                "      result = e.getClass().getSimpleName() + \": \" + String.valueOf(e.getMessage()).split(\"\\n\")[0];",
                "    }",
                "    RESULTS.put(name, result);",
                "  }",
                "  /** Works around the Yasson limitation of testYassonContainerLimitation: reads the types first. */",
                "  static void readTypesFirst(Jsonb j, Class<?>... types) {",
                "    for (Class<?> type : types) {",
                "      j.fromJson(\"null\", type);",
                "    }",
                "  }",
                "  public static java.util.Map<String, String> run() throws Exception {",
                "    checks();",
                "    return RESULTS;",
                "  }"));
        lines.addAll(Arrays.asList(body));
        lines.add("}");
        Path checks = writeSource(output.resolve("checks"), className, lines.toArray(new String[0]));
        assertCompiles(checks, classes, "17", classpath);
        try (URLClassLoader loader = loader(classes)) {
            @SuppressWarnings("unchecked")
            Map<String, String> results = (Map<String, String>) loader.loadClass(MODEL_PACKAGE + "." + className).getMethod("run").invoke(null);
            return new LinkedHashMap<>(results);
        }
    }

    /** Like assertEquals, but reports every differing check instead of the first one only. */
    private static void assertResults(Map<String, String> results, Map<String, String> expected) {
        Map<String, String> differences = new LinkedHashMap<>();
        Set<String> checks = new LinkedHashSet<>(expected.keySet());
        checks.addAll(results.keySet());
        for (String check : checks) {
            if (!Objects.equals(results.get(check), expected.get(check))) {
                differences.put(check, "expected <" + expected.get(check) + "> but was <" + results.get(check) + ">");
            }
        }
        assertTrue(differences.isEmpty(), "differing checks: " + differences);
    }

    /**
     * Expects the given result of an existing check under Johnzon, a known deviation of Johnzon (2.0.2, 2.2.0) that is
     * reproduced without generated code: it does not apply the class-level deserializer of an enum to properties
     * (2.0.2 also to creator parameters, while 2.2.0 passes them a parser without next()), it applies the class-level
     * deserializer of a class also when a subclass is read, it rejects JSON of the wrong type before calling a
     * deserializer, and it fails before calling a deserializer when a creator parameter with one is missing.
     */
    private static void expectUnderJohnzon(Map<String, String> expected, String check, String result) {
        expectUnderJohnzon(expected, "", check, result);
    }

    /** As {@link #expectUnderJohnzon(Map, String, String)}, for the Johnzon versions that start with the given prefix. */
    private static void expectUnderJohnzon(Map<String, String> expected, String versionPrefix, String check, String result) {
        assertTrue(expected.containsKey(check), "no check " + check);
        if (johnzon() && String.valueOf(JsonbProvider.provider().getClass().getPackage().getImplementationVersion()).startsWith(versionPrefix)) {
            expected.put(check, result);
        }
    }

    /** Whether the JSON-B implementation of the test classpath is Johnzon, whose known deviations some checks expect. */
    private static boolean johnzon() {
        return JsonbProvider.provider().getClass().getName().startsWith("org.apache.johnzon.");
    }

    private static String readModel(Path model) {
        try {
            return new String(Files.readAllBytes(model), StandardCharsets.UTF_8);
        } catch (IOException e) {
            throw new UncheckedIOException(e);
        }
    }

    private static String classDeclaration(Path model) throws Exception {
        return Files.readAllLines(model).stream().filter(line -> line.startsWith("public ")).findFirst().orElse("");
    }

    private static Map<String, Object> properties(boolean legacyDiscriminatorBehavior) {
        Map<String, Object> properties = new HashMap<>();
        properties.put(CodegenConstants.SERIALIZATION_LIBRARY, JavaJAXRSSpecServerCodegen.SERIALIZATION_LIBRARY_JSONB);
        properties.put(JavaJAXRSSpecServerCodegen.JSONB_DISCRIMINATOR_DESERIALIZERS, "true");
        properties.put("useSealed", "true");
        properties.put("generateBuilders", "true");
        properties.put(JavaJAXRSSpecServerCodegen.GENERATE_JSON_CREATOR, "true");
        properties.put(AbstractJavaJAXRSServerCodegen.USE_JAKARTA_EE, "true");
        properties.put(CodegenConstants.LEGACY_DISCRIMINATOR_BEHAVIOR, String.valueOf(legacyDiscriminatorBehavior));
        properties.put("useSwaggerAnnotations", "false");
        properties.put("useBeanValidation", "false");
        return properties;
    }
}
