package org.openapitools.codegen.languages.features;

import java.io.PrintWriter;
import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;
import org.openapitools.codegen.languages.features.DocumentationProviderFeatures.AnnotationLibrary;
import org.openapitools.codegen.languages.features.DocumentationProviderFeatures.DocumentationProvider;
import org.testng.annotations.AfterTest;
import org.testng.annotations.Test;

public class DocumentationProviderFeaturesTest {

  PrintWriter writer = new PrintWriter(System.out);

  @AfterTest
  void flush() {
    writer.flush();
  }

  @Test(priority = 0)
  void generateDocumentationProviderTable() {
    writer.println("### DocumentationProvider\n");
    writer.println("|Cli Option|Description|Property Name|Preferred Annotation Library|Supported Annotation Libraries|");
    writer.println("|----------|-----------|-------------|----------------------------|------------------------------|");
    List<DocumentationProvider> providers = Arrays.asList(DocumentationProvider.values());
    providers.forEach(dp -> writer.printf("|**%s**|%s|`%s`|%s|%s|\n",
        dp.toCliOptValue(),
        dp.getDescription(),
        dp.getPropertyName(),
        dp.getPreferredAnnotationLibrary().toCliOptValue(),
        dp.supportedAnnotationLibraries().stream().map(AnnotationLibrary::toCliOptValue).collect(Collectors.joining(", "))
        ));
    writer.println();
  }

  @Test(priority = 1)
  void generateAnnotationLibraryTable() {
    writer.println("### AnnotationLibrary\n");
    writer.println("|Cli Option|Description|Property Name|");
    writer.println("|----------|-----------|-----------|");
    List<AnnotationLibrary> libraries = Arrays.asList(AnnotationLibrary.values());
    libraries.forEach(dp -> writer.printf("|**%s**|%s|`%s`|\n",
        dp.toCliOptValue(),
        dp.getDescription(),
        dp.getPropertyName()
    ));
    writer.println();
  }
}