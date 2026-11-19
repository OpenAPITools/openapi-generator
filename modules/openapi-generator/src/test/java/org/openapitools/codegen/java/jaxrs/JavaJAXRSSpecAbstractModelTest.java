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
import com.fasterxml.jackson.core.JsonFactory;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.exc.InvalidTypeIdException;
import jakarta.json.bind.Jsonb;
import jakarta.json.bind.JsonbBuilder;
import jakarta.json.stream.JsonParser;
import org.openapitools.codegen.CodegenConstants;
import org.openapitools.codegen.VendorExtension;
import org.openapitools.codegen.languages.AbstractJavaJAXRSServerCodegen;
import org.openapitools.codegen.languages.JavaJAXRSCXFCDIServerCodegen;
import org.openapitools.codegen.languages.JavaJAXRSSpecServerCodegen;
import org.testng.annotations.DataProvider;
import org.testng.annotations.Test;

import java.io.File;
import java.lang.reflect.Modifier;
import java.net.URLClassLoader;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;
import java.util.stream.Collectors;

import static org.openapitools.codegen.TestUtils.assertFileContains;
import static org.openapitools.codegen.TestUtils.assertFileNotContains;
import static org.openapitools.codegen.TestUtils.newTempFolder;
import static org.openapitools.codegen.java.jaxrs.JavaJAXRSSpecCompileSupport.CompileResult;
import static org.openapitools.codegen.java.jaxrs.JavaJAXRSSpecCompileSupport.MODEL_PACKAGE;
import static org.openapitools.codegen.java.jaxrs.JavaJAXRSSpecCompileSupport.SPEC_DIR;
import static org.openapitools.codegen.java.jaxrs.JavaJAXRSSpecCompileSupport.assertCompiles;
import static org.openapitools.codegen.java.jaxrs.JavaJAXRSSpecCompileSupport.classpath;
import static org.openapitools.codegen.java.jaxrs.JavaJAXRSSpecCompileSupport.compile;
import static org.openapitools.codegen.java.jaxrs.JavaJAXRSSpecCompileSupport.compileModels;
import static org.openapitools.codegen.java.jaxrs.JavaJAXRSSpecCompileSupport.generate;
import static org.openapitools.codegen.java.jaxrs.JavaJAXRSSpecCompileSupport.loader;
import static org.openapitools.codegen.java.jaxrs.JavaJAXRSSpecCompileSupport.modelDir;
import static org.openapitools.codegen.java.jaxrs.JavaJAXRSSpecCompileSupport.requireJdk;
import static org.openapitools.codegen.java.jaxrs.JavaJAXRSSpecCompileSupport.withoutGeneratedAnnotation;
import static org.openapitools.codegen.java.jaxrs.JavaJAXRSSpecCompileSupport.writeSource;
import static org.testng.Assert.*;

/**
 * Tests the {@code x-abstract} vendor extension of the jaxrs-spec generator and its combination with
 * {@code useSealed}: abstract non-leaf classes, final leaves, builders and JSON creators of abstract classes,
 * the rule that no discriminator value may resolve to an abstract class, sealed oneOf interfaces, and an
 * exhaustive pattern matching {@code switch} without {@code default} over the generated hierarchy. Every
 * generation runs with {@code legacyDiscriminatorBehavior} {@code true} (default) and {@code false}, because the
 * latter copies the parent discriminator into its children.
 * <p>
 * Assertions on sealed sources use {@code assertFileContains} because {@code JavaFileAssert} parses with a
 * JavaParser language level that predates sealed/permits. The compile tests need a JDK 17 (sealed classes) or
 * JDK 21 (pattern matching for switch) and are skipped on older JDKs.
 */
public class JavaJAXRSSpecAbstractModelTest {

    private static final String SPEC = SPEC_DIR + "abstract_sealed_hierarchy.yaml";

    @DataProvider
    public Object[][] legacyDiscriminatorBehavior() {
        return new Object[][]{{true}, {false}};
    }

    @Test(dataProvider = "legacyDiscriminatorBehavior")
    public void testAbstractSealedHierarchyGeneration(boolean legacy) throws Exception {
        Path modelDir = modelDir(generate(SPEC, jacksonSealedProperties(legacy)));

        Path pet = modelDir.resolve("Pet.java");
        assertFileContains(pet,
                "public abstract sealed class Pet permits Cat, Dog {",
                "protected Pet(PetBuilder<?, ?> b) {",
                "protected Pet() {",
                "protected Pet( @JsonProperty(required = true, value = \"petType\") String petType,",
                "public static abstract class PetBuilder<C extends Pet, B extends PetBuilder<C, B>>",
                "@JsonTypeInfo(use = JsonTypeInfo.Id.NAME, include = JsonTypeInfo.As.PROPERTY, property = \"petType\", visible = true)",
                // the discriminator values map directly to the concrete leaves
                "@JsonSubTypes.Type(value = Dog.class, name = \"DOG\")",
                "@JsonSubTypes.Type(value = PersianCat.class, name = \"PERSIAN\")",
                "@JsonSubTypes.Type(value = SiameseCat.class, name = \"SIAMESE\")");
        assertFileNotContains(pet, "@JsonCreator", "public static PetBuilder<?, ?> builder()", "PetBuilderImpl",
                "final class Pet", "value = Cat.class");

        Path cat = modelDir.resolve("Cat.java");
        assertFileContains(cat,
                "public abstract sealed class Cat extends Pet permits PersianCat, SiameseCat {",
                "protected Cat() {",
                "public static abstract class CatBuilder<C extends Cat, B extends CatBuilder<C, B>> extends PetBuilder<C, B>");
        assertFileNotContains(cat, "@JsonCreator", "public static CatBuilder<?, ?> builder()", "CatBuilderImpl", "value = Cat.class");

        assertFileContains(modelDir.resolve("PersianCat.java"),
                "@JsonTypeName(\"PERSIAN\")",
                "public final class PersianCat extends Cat {",
                "public PersianCat() {",
                "@JsonCreator public PersianCat(",
                "public static PersianCatBuilder<?, ?> builder() {",
                "return new PersianCat(this);");
        assertFileContains(modelDir.resolve("SiameseCat.java"), "@JsonTypeName(\"SIAMESE\")", "public final class SiameseCat extends Cat {");
        assertFileContains(modelDir.resolve("Dog.java"),
                "@JsonTypeName(\"DOG\")",
                "public final class Dog extends Pet {",
                "@JsonCreator public Dog(",
                "public static DogBuilder<?, ?> builder() {");

        // abstract without subclasses outside a sealed hierarchy: abstract, not final ("final abstract" does not compile)
        assertFileContains(modelDir.resolve("Shape.java"), "public abstract class Shape {");
        assertFileNotContains(modelDir.resolve("Shape.java"), "final class Shape", "sealed class Shape", "permits ");
        // x-abstract: "false" (a string) must not make the model abstract
        assertFileContains(modelDir.resolve("Toy.java"), "public final class Toy {", "public Toy() {");
        assertFileNotContains(modelDir.resolve("Toy.java"), "public abstract class Toy", "protected Toy() {");
    }

    @Test
    public void testAbstractWithoutUseSealed() throws Exception {
        Map<String, Object> properties = jacksonSealedProperties(true);
        properties.remove("useSealed");
        Path modelDir = modelDir(generate(SPEC, properties));

        assertFileContains(modelDir.resolve("Pet.java"), "public abstract class Pet {", "protected Pet() {");
        assertFileContains(modelDir.resolve("Cat.java"), "public abstract class Cat extends Pet {");
        assertFileContains(modelDir.resolve("PersianCat.java"), "public class PersianCat extends Cat {");
        assertFileContains(modelDir.resolve("Dog.java"), "public class Dog extends Pet {");
        for (String model : Arrays.asList("Pet", "Cat", "PersianCat", "Dog")) {
            assertFileNotContains(modelDir.resolve(model + ".java"), "sealed class", "permits ", "public final class");
        }
    }

    /**
     * Implicit mappings (allOf descendants mapped under their schema name) to abstract models are dropped: only the
     * concrete classes are subtypes, which Jackson resolves through the root type; the abstract type name is no type id.
     * Explicit mappings are covered by the rejection tests.
     */
    @Test(dataProvider = "legacyDiscriminatorBehavior")
    public void testImplicitMappingsToAbstractModelsAreDropped(boolean legacy) throws Exception {
        Path output = generate(SPEC_DIR + "abstract_implicit_mapping.yaml", jacksonSealedProperties(legacy));
        Path modelDir = modelDir(output);
        assertFileContains(modelDir.resolve("Pet.java"),
                "public abstract sealed class Pet permits Cat, Dog {",
                "@JsonSubTypes.Type(value = Dog.class, name = \"Dog\")",
                "@JsonSubTypes.Type(value = PersianCat.class, name = \"PersianCat\")",
                "@JsonSubTypes.Type(value = SiameseCat.class, name = \"SiameseCat\")");
        assertFileNotContains(modelDir.resolve("Pet.java"), "value = Cat.class");
        assertFileNotContains(modelDir.resolve("Cat.java"), "value = Cat.class");
        assertFileContains(modelDir.resolve("Cat.java"), "public abstract sealed class Cat extends Pet permits PersianCat, SiameseCat {");

        requireJdk(17);
        Path classes = compileModels(output, classpath(JsonProperty.class));
        try (URLClassLoader loader = loader(classes)) {
            Class<?> pet = loader.loadClass(MODEL_PACKAGE + ".Pet");
            ObjectMapper mapper = new ObjectMapper();
            assertEquals(mapper.readValue("{\"petType\":\"PersianCat\"}", pet).getClass().getSimpleName(), "PersianCat");
            assertEquals(mapper.readValue("{\"petType\":\"Dog\"}", pet).getClass().getSimpleName(), "Dog");
            expectThrows(InvalidTypeIdException.class, () -> mapper.readValue("{\"petType\":\"Cat\"}", pet));
        }
    }

    @DataProvider
    public Object[][] rejectedSpecs() {
        List<Object[]> cases = new ArrayList<>();
        for (boolean legacy : new boolean[]{true, false}) {
            cases.add(new Object[]{"abstract_self_mapping.yaml", legacy,
                    "The discriminator value 'PET' of property 'petType' in model 'Pet' maps to model 'Pet', which is marked 'x-abstract'"});
            cases.add(new Object[]{"abstract_mapped_leaf.yaml", legacy,
                    "The discriminator value 'DOG' of property 'petType' in model 'Pet' maps to model 'Dog', which is marked 'x-abstract'"});
            // an abstract intermediate type resolved by a second discriminator is not readable through the root type
            cases.add(new Object[]{"abstract_mapped_intermediate.yaml", legacy,
                    "The discriminator value 'CAT' of property 'petType' in model 'Pet' maps to model 'Cat', which is marked 'x-abstract'"});
            cases.add(new Object[]{"abstract_sealed_parent_without_subclass.yaml", legacy,
                    "Model 'Fish' is marked 'x-abstract' and is a permitted subtype of the sealed type 'Pet', but no generated model extends it"});
            cases.add(new Object[]{"abstract_sealed_oneof_member_without_subclass.yaml", legacy,
                    "Model 'CatRequest' is marked 'x-abstract' and is a permitted subtype of the sealed type 'PetRequest', but no generated model extends it"});
        }
        // a two-level discriminator below an implicitly mapped abstract oneOf member (only legacyDiscriminatorBehavior=false
        // maps oneOf members implicitly; with true the interface lists no subtypes, as without x-abstract)
        cases.add(new Object[]{"abstract_oneof_member_other_discriminator.yaml", false,
                "Model 'PersianCatRequest' extends model 'CatRequest', which is marked 'x-abstract' and is implicitly mapped by the "
                        + "discriminator property 'petType' of model 'PetRequest'. Model 'CatRequest' on this inheritance path "
                        + "resolves its subclasses by the discriminator property 'catType'"});
        // the same on a deeper abstract level of the inheritance path
        cases.add(new Object[]{"abstract_oneof_deep_other_discriminator.yaml", false,
                "Model 'BigCatRequest' on this inheritance path resolves its subclasses by the discriminator property 'bigCatType', "
                        + "so model 'LionRequest' cannot be mapped by 'petType' instead"});
        return cases.toArray(new Object[0][]);
    }

    @Test(dataProvider = "rejectedSpecs")
    public void testInvalidAbstractModelsAreRejected(String spec, boolean legacy, String message) {
        Map<String, Object> properties = jacksonSealedProperties(legacy);
        properties.put("useOneOfInterfaces", "true");
        IllegalArgumentException e = expectThrows(IllegalArgumentException.class, () -> generate(SPEC_DIR + spec, properties));
        assertTrue(e.getMessage().contains(message), e.getMessage());
    }

    @DataProvider
    public Object[][] abstractWithoutSubclassSpecs() {
        return new Object[][]{
                {"abstract_sealed_parent_without_subclass.yaml", true},
                {"abstract_sealed_parent_without_subclass.yaml", false},
                {"abstract_sealed_oneof_member_without_subclass.yaml", true},
                {"abstract_sealed_oneof_member_without_subclass.yaml", false}};
    }

    /**
     * Without {@code useSealed} an abstract model without generated subclasses is allowed (it may be extended by
     * hand-written code) and compiles.
     */
    @Test(dataProvider = "abstractWithoutSubclassSpecs")
    public void testAbstractWithoutSubclassCompilesWithoutUseSealed(String spec, boolean legacy) throws Exception {
        requireJdk(17);
        Map<String, Object> properties = jacksonSealedProperties(legacy);
        properties.remove("useSealed");
        properties.put("useOneOfInterfaces", "true");
        Path output = generate(SPEC_DIR + spec, properties);
        compileModels(output, classpath(JsonProperty.class));
    }

    /**
     * The generated Jackson models compile for Java 17, the class files carry the expected abstract/sealed/final
     * structure, and Jackson resolves every concrete subtype through the root type and through the abstract
     * intermediate type, also when serializing a builder-built leaf and reading it back.
     */
    @Test(dataProvider = "legacyDiscriminatorBehavior")
    public void testAbstractSealedJacksonModelsCompileAndBind(boolean legacy) throws Exception {
        requireJdk(17);
        Path output = generate(SPEC, jacksonSealedProperties(legacy));
        String classpath = classpath(JsonProperty.class, ObjectMapper.class, JsonFactory.class);
        Path classes = compileModels(output, classpath);
        Path checks = writeSource(output.resolve("checks"), "AbstractModelChecks",
                "package " + MODEL_PACKAGE + ";",
                "import com.fasterxml.jackson.databind.ObjectMapper;",
                "public final class AbstractModelChecks {",
                "  private static final ObjectMapper MAPPER = new ObjectMapper();",
                "  static String read(Class<?> type, String petType) {",
                "    try {",
                "      return MAPPER.readValue(\"{\\\"petType\\\":\\\"\" + petType + \"\\\",\\\"name\\\":\\\"n\\\"}\", type).getClass().getSimpleName();",
                "    } catch (Exception e) {",
                "      return e.getClass().getSimpleName();",
                "    }",
                "  }",
                "  public static String run() throws Exception {",
                "    PersianCat built = PersianCat.builder().name(\"tom\").longHair(true).build();",
                "    String json = MAPPER.writeValueAsString(built);",
                "    return String.join(\";\",",
                "        read(Pet.class, \"DOG\"), read(Pet.class, \"PERSIAN\"), read(Pet.class, \"SIAMESE\"),",
                "        read(Cat.class, \"PERSIAN\"), read(Cat.class, \"DOG\"), read(Pet.class, \"Cat\"),",
                "        MAPPER.readValue(json, Pet.class).getClass().getSimpleName(),",
                "        new Dog(\"DOG\", \"rex\").getName(),",
                "        json);",
                "  }",
                "}");
        assertCompiles(checks, classes, "17", classpath);

        try (URLClassLoader loader = loader(classes)) {
            assertAbstractSealed(loader.loadClass(MODEL_PACKAGE + ".Pet"), "Cat", "Dog");
            assertAbstractSealed(loader.loadClass(MODEL_PACKAGE + ".Cat"), "PersianCat", "SiameseCat");
            for (String leaf : Arrays.asList("Dog", "PersianCat", "SiameseCat", "Toy")) {
                assertTrue(Modifier.isFinal(loader.loadClass(MODEL_PACKAGE + "." + leaf).getModifiers()), leaf + " must be final");
            }
            Class<?> shape = loader.loadClass(MODEL_PACKAGE + ".Shape");
            assertTrue(Modifier.isAbstract(shape.getModifiers()) && !Modifier.isFinal(shape.getModifiers()));

            String result = (String) loader.loadClass(MODEL_PACKAGE + ".AbstractModelChecks").getMethod("run").invoke(null);
            List<String> parts = Arrays.asList(result.split(";", 9));
            assertEquals(parts.subList(0, 8), Arrays.asList(
                    "Dog", "PersianCat", "SiameseCat",                          // through the root type
                    "PersianCat", "InvalidTypeIdException",                     // through the abstract intermediate type
                    "InvalidTypeIdException",                                   // the abstract type itself is not a subtype id
                    "PersianCat",                                               // builder -> JSON -> root type
                    "rex"), result);
            assertTrue(parts.get(8).contains("\"petType\":\"PERSIAN\"") && parts.get(8).contains("\"longHair\":true"), result);
        }
    }

    @DataProvider
    public Object[][] oneOfDiscriminatorCases() {
        return new Object[][]{
                // implicit mapping: only legacyDiscriminatorBehavior=false maps oneOf members (see the legacy test below)
                {"abstract_oneof_discriminator.yaml", false, "PersianCatRequest", "SiameseCatRequest", "DogRequest"},
                {"abstract_oneof_discriminator_explicit.yaml", true, "PERSIAN", "SIAMESE", "DOG"},
                {"abstract_oneof_discriminator_explicit.yaml", false, "PERSIAN", "SIAMESE", "DOG"}};
    }

    /**
     * A oneOf interface with a discriminator whose member is abstract: the abstract member is not a subtype id, but
     * its concrete subclasses are listed on the interface, so Jackson resolves them through the root interface (also
     * after a builder -> JSON roundtrip). The sealed hierarchy is unchanged: the interface still permits the abstract
     * member, which permits its leaves.
     */
    @Test(dataProvider = "oneOfDiscriminatorCases")
    public void testAbstractOneOfMemberDescendantsBindThroughRootInterface(String spec, boolean legacy, String persian,
                                                                           String siamese, String dog) throws Exception {
        requireJdk(17);
        Path output = generate(SPEC_DIR + spec, oneOfProperties(legacy));
        Path modelDir = modelDir(output);
        assertFileContains(modelDir.resolve("PetRequest.java"),
                "public sealed interface PetRequest permits CatRequest, DogRequest {",
                "@JsonSubTypes.Type(value = PersianCatRequest.class, name = \"" + persian + "\")",
                "@JsonSubTypes.Type(value = SiameseCatRequest.class, name = \"" + siamese + "\")",
                "@JsonSubTypes.Type(value = DogRequest.class, name = \"" + dog + "\")");
        assertFileNotContains(modelDir.resolve("PetRequest.java"), "value = CatRequest.class");
        assertFileContains(modelDir.resolve("CatRequest.java"),
                "public abstract sealed class CatRequest implements PetRequest permits PersianCatRequest, SiameseCatRequest {");
        assertFileContains(modelDir.resolve("PersianCatRequest.java"), "@JsonTypeName(\"" + persian + "\")");

        String classpath = classpath(JsonProperty.class, ObjectMapper.class, JsonFactory.class);
        Path classes = compileModels(output, classpath);
        Path checks = writeSource(output.resolve("checks"), "OneOfChecks",
                "package " + MODEL_PACKAGE + ";",
                "import com.fasterxml.jackson.databind.ObjectMapper;",
                "public final class OneOfChecks {",
                "  private static final ObjectMapper MAPPER = new ObjectMapper();",
                "  static String read(String petType) {",
                "    try {",
                "      return MAPPER.readValue(\"{\\\"petType\\\":\\\"\" + petType + \"\\\"}\", PetRequest.class).getClass().getSimpleName();",
                "    } catch (Exception e) {",
                "      return e.getClass().getSimpleName();",
                "    }",
                "  }",
                "  public static String run() throws Exception {",
                "    String json = MAPPER.writeValueAsString(PersianCatRequest.builder().longHair(true).build());",
                "    PetRequest read = MAPPER.readValue(json, PetRequest.class);",
                "    return String.join(\";\", read(\"" + persian + "\"), read(\"" + siamese + "\"), read(\"" + dog + "\"),",
                "        read(\"CatRequest\"), read.getClass().getSimpleName(), String.valueOf(((PersianCatRequest) read).getLongHair()), json);",
                "  }",
                "}");
        assertCompiles(checks, classes, "17", classpath);
        try (URLClassLoader loader = loader(classes)) {
            String result = (String) loader.loadClass(MODEL_PACKAGE + ".OneOfChecks").getMethod("run").invoke(null);
            List<String> parts = Arrays.asList(result.split(";", 7));
            assertEquals(parts.subList(0, 6), Arrays.asList(
                    "PersianCatRequest", "SiameseCatRequest", "DogRequest",   // through the root interface
                    "InvalidTypeIdException",                                 // the abstract member is not a subtype id
                    "PersianCatRequest", "true"),                             // builder -> JSON -> root interface
                    result);
            assertTrue(parts.get(6).contains("\"petType\":\"" + persian + "\""), result);
        }
    }

    /**
     * Two abstract levels below an implicitly mapped oneOf member that dispatch by the same property. Without
     * {@code x-abstract}, Jackson resolves the leaf transitively under every value that the intermediate models map it
     * to ("LionRequest" from CatRequest, "LION" from BigCatRequest). With {@code x-abstract} the leaf must stay
     * resolvable under exactly the same values through every type, only the abstract types themselves are no longer
     * subtype ids.
     */
    @Test
    public void testDeepAbstractOneOfMemberWithSameDiscriminatorPropertyBinds() throws Exception {
        requireJdk(17);
        String spec = SPEC_DIR + "abstract_oneof_deep_discriminator.yaml";
        Path output = generate(spec, oneOfProperties(false));
        Path modelDir = modelDir(output);
        assertFileContains(modelDir.resolve("PetRequest.java"),
                "public sealed interface PetRequest permits CatRequest, DogRequest {",
                "@JsonSubTypes.Type(value = LionRequest.class, name = \"LION\")",
                "@JsonSubTypes.Type(value = LionRequest.class, name = \"LionRequest\")");
        assertFileNotContains(modelDir.resolve("PetRequest.java"), "value = CatRequest.class", "value = BigCatRequest.class");

        Path specWithoutAbstract = newTempFolder().resolve("without_abstract.yaml");
        Files.write(specWithoutAbstract, Files.readAllLines(Paths.get(spec)).stream()
                .filter(line -> !line.contains("x-abstract"))
                .collect(Collectors.toList()));
        List<String> types = Arrays.asList("PetRequest", "CatRequest", "BigCatRequest");
        List<String> leafIds = Arrays.asList("LION", "LionRequest", "DogRequest");
        Map<String, String> withAbstract = readThroughTypes(output, types, leafIds);
        Map<String, String> withoutAbstract = readThroughTypes(generate(specWithoutAbstract.toString(), oneOfProperties(false)), types, leafIds);
        assertEquals(withAbstract, withoutAbstract);
        assertEquals(withAbstract.get("PetRequest<-LION"), "LionRequest");
        assertEquals(withAbstract.get("CatRequest<-LION"), "LionRequest");
        assertEquals(withAbstract.get("PetRequest<-LionRequest"), "LionRequest");

        Path classes = output.resolve("classes");
        try (URLClassLoader loader = loader(classes)) {
            Class<?> petRequest = loader.loadClass(MODEL_PACKAGE + ".PetRequest");
            ObjectMapper mapper = new ObjectMapper();
            for (String abstractType : Arrays.asList("CatRequest", "BigCatRequest")) {
                expectThrows(InvalidTypeIdException.class,
                        () -> mapper.readValue("{\"petType\":\"" + abstractType + "\"}", petRequest));
            }
        }
    }

    /**
     * Compiles the generated models and reads {"petType": id} through each type with Jackson; the result maps
     * "Type<-id" to the simple name of the read class or of the exception.
     */
    private static Map<String, String> readThroughTypes(Path output, List<String> types, List<String> ids) throws Exception {
        Path classes = compileModels(output, classpath(JsonProperty.class));
        Map<String, String> results = new TreeMap<>();
        try (URLClassLoader loader = loader(classes)) {
            ObjectMapper mapper = new ObjectMapper();
            for (String type : types) {
                Class<?> target = loader.loadClass(MODEL_PACKAGE + "." + type);
                for (String id : ids) {
                    String result;
                    try {
                        result = mapper.readValue("{\"petType\":\"" + id + "\"}", target).getClass().getSimpleName();
                    } catch (com.fasterxml.jackson.core.JsonProcessingException e) {
                        result = e.getClass().getSimpleName();
                    }
                    results.put(type + "<-" + id, result);
                }
            }
        }
        return results;
    }

    /**
     * Existing limitation, not caused by {@code x-abstract}: with {@code legacyDiscriminatorBehavior=true} a oneOf
     * interface without explicit mapping lists no subtypes at all, so Jackson cannot read any member through it. The
     * interface is generated exactly as without {@code x-abstract}.
     */
    @Test
    public void testImplicitOneOfMappingInLegacyModeIsUnchangedByAbstract() throws Exception {
        Path withAbstract = modelDir(generate(SPEC_DIR + "abstract_oneof_discriminator.yaml", oneOfProperties(true)));
        Path specWithoutAbstract = newTempFolder().resolve("without_abstract.yaml");
        Files.write(specWithoutAbstract, Files.readAllLines(Paths.get(SPEC_DIR + "abstract_oneof_discriminator.yaml")).stream()
                .filter(line -> !line.contains("x-abstract"))
                .collect(Collectors.toList()));
        Path withoutAbstract = modelDir(generate(specWithoutAbstract.toString(), oneOfProperties(true)));

        assertFileNotContains(withAbstract.resolve("PetRequest.java"), "@JsonSubTypes.Type");
        assertEquals(withoutGeneratedAnnotation(withAbstract.resolve("PetRequest.java")),
                withoutGeneratedAnnotation(withoutAbstract.resolve("PetRequest.java")));
        assertFileContains(withAbstract.resolve("CatRequest.java"), "public abstract sealed class CatRequest");
        assertFileContains(withoutAbstract.resolve("CatRequest.java"), "public sealed class CatRequest");
    }

    /**
     * With {@code useSealed} and {@code x-abstract} on every non-leaf class, a switch over the root type that lists
     * only the leaves is exhaustive and needs no {@code default}; leaving a leaf out must be a compile error, which
     * proves that the compiler checks exhaustiveness. The models are compiled for Java 17; the switch itself needs
     * Java 21 (pattern matching for switch is a preview feature in Java 17).
     */
    @Test(dataProvider = "legacyDiscriminatorBehavior")
    public void testExhaustiveSwitchOverAbstractClassHierarchy(boolean legacy) throws Exception {
        requireJdk(21);
        assertExhaustiveSwitch(SPEC, legacy, "Pet", Arrays.asList("Dog", "PersianCat", "SiameseCat"));
    }

    /**
     * A sealed oneOf interface whose member is an abstract sealed class: the switch over the interface lists the
     * concrete leaves below the abstract member.
     */
    @Test(dataProvider = "legacyDiscriminatorBehavior")
    public void testExhaustiveSwitchOverSealedOneOfInterface(boolean legacy) throws Exception {
        requireJdk(21);
        Path modelDir = modelDir(generate(SPEC_DIR + "abstract_sealed_oneof.yaml", oneOfProperties(legacy)));
        assertFileContains(modelDir.resolve("PetRequest.java"), "public sealed interface PetRequest permits CatRequest, DogRequest {");
        assertFileContains(modelDir.resolve("CatRequest.java"),
                "public abstract sealed class CatRequest implements PetRequest permits PersianCatRequest, SiameseCatRequest {");
        assertFileContains(modelDir.resolve("PersianCatRequest.java"), "public final class PersianCatRequest extends CatRequest {");
        assertFileContains(modelDir.resolve("DogRequest.java"), "public final class DogRequest implements PetRequest {");

        assertExhaustiveSwitch(SPEC_DIR + "abstract_sealed_oneof.yaml", legacy, "PetRequest",
                Arrays.asList("DogRequest", "PersianCatRequest", "SiameseCatRequest"));
    }

    /**
     * The abstract JSON-B classes carry no {@code @JsonbCreator}; the concrete classes compile for Java 17 and Yasson
     * binds and serializes them through their creators.
     */
    @Test(dataProvider = "legacyDiscriminatorBehavior")
    public void testAbstractSealedJsonbModels(boolean legacy) throws Exception {
        Path output = generate(SPEC, jsonbSealedProperties(legacy));
        Path modelDir = modelDir(output);
        assertFileContains(modelDir.resolve("Pet.java"),
                "public abstract sealed class Pet permits Cat, Dog {",
                "protected Pet( @JsonbProperty(\"petType\") String petType,");
        assertFileNotContains(modelDir.resolve("Pet.java"), "@JsonbCreator", "public static PetBuilder<?, ?> builder()");
        assertFileNotContains(modelDir.resolve("Cat.java"), "@JsonbCreator");
        assertFileContains(modelDir.resolve("PersianCat.java"), "@JsonbCreator public PersianCat(");
        assertFileContains(modelDir.resolve("Dog.java"), "@JsonbCreator public Dog(");

        requireJdk(17);
        Path classes = compileModels(output, classpath(Jsonb.class, JsonParser.class));
        try (URLClassLoader loader = loader(classes);
             Jsonb jsonb = JsonbBuilder.create()) {
            Class<?> persianCat = loader.loadClass(MODEL_PACKAGE + ".PersianCat");
            Object cat = jsonb.fromJson("{\"petType\":\"PERSIAN\",\"name\":\"tom\",\"longHair\":true}", persianCat);
            assertEquals(cat.getClass(), persianCat);
            assertEquals(persianCat.getMethod("getName").invoke(cat), "tom");
            assertEquals(persianCat.getMethod("getLongHair").invoke(cat), Boolean.TRUE);
            String json = jsonb.toJson(cat);
            assertTrue(json.contains("\"petType\":\"PERSIAN\"") && json.contains("\"longHair\":true"), json);
        }
    }

    @Test
    public void testXAbstractIsAdvertisedByJaxrsSpecOnly() {
        assertTrue(new JavaJAXRSSpecServerCodegen().getSupportedVendorExtensions().contains(VendorExtension.X_ABSTRACT));
        // cxf-cdi has its own templates that do not implement x-abstract
        assertFalse(new JavaJAXRSCXFCDIServerCodegen().getSupportedVendorExtensions().contains(VendorExtension.X_ABSTRACT));
    }

    private void assertExhaustiveSwitch(String spec, boolean legacy, String rootType, List<String> leaves) throws Exception {
        Map<String, Object> properties = rootType.equals("Pet") ? jacksonSealedProperties(legacy) : oneOfProperties(legacy);
        Path output = generate(spec, properties);
        String classpath = classpath(JsonProperty.class);
        Path classes = compileModels(output, classpath);

        Path exhaustive = writeSource(output.resolve("switch"), "Switch", switchSource(rootType, leaves));
        assertCompiles(exhaustive, classes, "21", classpath);
        try (URLClassLoader loader = loader(classes)) {
            Object result = loader.loadClass(MODEL_PACKAGE + ".Switch").getMethod("run").invoke(null);
            assertEquals(result, String.join(";", leaves));
        }

        Path incomplete = writeSource(output.resolve("switch-incomplete"), "Switch", switchSource(rootType, leaves.subList(0, leaves.size() - 1)));
        CompileResult missingLeaf = compile(incomplete, output.resolve("switch-incomplete-classes"), "21", classes + File.pathSeparator + classpath);
        assertNotEquals(missingLeaf.exitCode, 0, "a switch without the " + leaves.get(leaves.size() - 1) + " case must not compile");
        assertTrue(missingLeaf.diagnostics.contains("does not cover all possible input values"), missingLeaf.diagnostics);
    }

    /**
     * A switch over the root type with one case per given leaf, and a {@code run()} that passes an instance of each
     * leaf through it.
     */
    private static String[] switchSource(String rootType, List<String> leaves) {
        List<String> lines = new ArrayList<>(Arrays.asList(
                "package " + MODEL_PACKAGE + ";",
                "public final class Switch {",
                "  static String describe(" + rootType + " value) {",
                "    return switch (value) {"));
        for (String leaf : leaves) {
            lines.add("      case " + leaf + " leaf -> \"" + leaf + "\";");
        }
        lines.addAll(Arrays.asList(
                "    };",
                "  }",
                "  public static String run() {",
                "    return String.join(\";\""));
        for (String leaf : leaves) {
            lines.add("        , describe(new " + leaf + "())");
        }
        lines.addAll(Arrays.asList("    );", "  }", "}"));
        return lines.toArray(new String[0]);
    }

    private static Map<String, Object> jacksonSealedProperties(boolean legacyDiscriminatorBehavior) {
        Map<String, Object> properties = new HashMap<>();
        properties.put("useSealed", "true");
        properties.put("generateBuilders", "true");
        properties.put(JavaJAXRSSpecServerCodegen.GENERATE_JSON_CREATOR, "true");
        properties.put(AbstractJavaJAXRSServerCodegen.USE_JAKARTA_EE, "true");
        properties.put(CodegenConstants.LEGACY_DISCRIMINATOR_BEHAVIOR, String.valueOf(legacyDiscriminatorBehavior));
        properties.put("openApiNullable", "false");
        properties.put("useSwaggerAnnotations", "false");
        properties.put("useBeanValidation", "false");
        return properties;
    }

    private static Map<String, Object> jsonbSealedProperties(boolean legacyDiscriminatorBehavior) {
        Map<String, Object> properties = jacksonSealedProperties(legacyDiscriminatorBehavior);
        properties.put(CodegenConstants.SERIALIZATION_LIBRARY, JavaJAXRSSpecServerCodegen.SERIALIZATION_LIBRARY_JSONB);
        properties.remove("openApiNullable");
        return properties;
    }

    private static Map<String, Object> oneOfProperties(boolean legacyDiscriminatorBehavior) {
        Map<String, Object> properties = jacksonSealedProperties(legacyDiscriminatorBehavior);
        properties.put("useOneOfInterfaces", "true");
        return properties;
    }

    /**
     * Asserts the structure an exhaustive switch relies on. {@code Class.isSealed()} and
     * {@code Class.getPermittedSubclasses()} are Java 17 API, so they are called reflectively: this test class is
     * compiled for Java 11.
     */
    private static void assertAbstractSealed(Class<?> type, String... permitted) throws Exception {
        assertTrue(Modifier.isAbstract(type.getModifiers()), type + " must be abstract");
        assertEquals(Class.class.getMethod("isSealed").invoke(type), Boolean.TRUE, type + " must be sealed");
        Class<?>[] permittedSubclasses = (Class<?>[]) Class.class.getMethod("getPermittedSubclasses").invoke(type);
        assertEquals(Arrays.stream(permittedSubclasses).map(Class::getSimpleName).sorted().collect(Collectors.toList()),
                Arrays.asList(permitted));
    }
}
