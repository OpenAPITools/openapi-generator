package org.openapitools.codegen.java.micronaut;

import org.openapitools.codegen.CodegenConstants;
import org.openapitools.codegen.languages.JavaMicronautAbstractCodegen;
import org.openapitools.codegen.languages.JavaMicronautClientCodegen;
import org.testng.annotations.Test;

public class JavaMicronautClientCodegenSerializationLibraryTest extends AbstractMicronautCodegenTest {

    @Test
    public void testSerializationLibraryJackson() {
        JavaMicronautClientCodegen codegen = new JavaMicronautClientCodegen();
        codegen.additionalProperties().put(CodegenConstants.SERIALIZATION_LIBRARY, JavaMicronautAbstractCodegen.SERIALIZATION_LIBRARY_TYPE.jackson.name());
        String outputPath = generateFiles(codegen, PETSTORE_PATH,
                                          CodegenConstants.MODELS);

        // Model does not contain micronaut serde annotation
        String micronautSerDeAnnotation = "@io.micronaut.serde.annotation.Serdeable";
        String modelPath = outputPath + "src/main/java/org/openapitools/model/";
        assertFileNotContains(modelPath + "Pet.java", micronautSerDeAnnotation);
        assertFileNotContains(modelPath + "User.java", micronautSerDeAnnotation);
        assertFileNotContains(modelPath + "Order.java", micronautSerDeAnnotation);
        assertFileNotContains(modelPath + "Tag.java", micronautSerDeAnnotation);
        assertFileNotContains(modelPath + "Category.java", micronautSerDeAnnotation);

        //JsonFormat with jackson must be with shape attribute
        assertFileContains(modelPath + "Order.java", "@JsonFormat(shape = JsonFormat.Shape.STRING");
    }

    /**
     * Checks micronaut-serde-jackson limitation.
     * @see <a href="https://micronaut-projects.github.io/micronaut-serialization/latest/guide/index.html#jacksonAnnotations"></a>
     */
    @Test
    public void testSerializationLibraryMicronautSerdeJackson() {
        JavaMicronautClientCodegen codegen = new JavaMicronautClientCodegen();
        codegen.additionalProperties().put(CodegenConstants.SERIALIZATION_LIBRARY, JavaMicronautAbstractCodegen.SERIALIZATION_LIBRARY_TYPE.micronaut_serde_jackson.name());
        String outputPath = generateFiles(codegen, PETSTORE_PATH,
                                          CodegenConstants.MODELS);

        // Model contains micronaut serde annotation
        String micronautSerDeAnnotation = "@io.micronaut.serde.annotation.Serdeable";
        String modelPath = outputPath + "src/main/java/org/openapitools/model/";
        assertFileContains(modelPath + "Pet.java", micronautSerDeAnnotation);
        assertFileContains(modelPath + "User.java", micronautSerDeAnnotation);
        assertFileContains(modelPath + "Order.java", micronautSerDeAnnotation);
        assertFileContains(modelPath + "Tag.java", micronautSerDeAnnotation);
        assertFileContains(modelPath + "Category.java", micronautSerDeAnnotation);

        //JsonFormat with micronaut-serde-jackson must be without shape attribute
        assertFileNotContains(modelPath + "Order.java", "@JsonFormat(shape = JsonFormat.Shape.STRING");
    }
}
