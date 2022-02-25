package org.openapitools.codegen.languages.features;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Locale;
import java.util.Objects;

/**
 * The DocumentationProvider Features support to additional properties to select the
 * documentation provider and the annotation library to use during code generation.
 *
 * @author cachescrubber, 2022-01-08
 * @since 5.4.0
 */
public interface DocumentationProviderFeatures {

  String DOCUMENTATION_PROVIDER = "documentationProvider";

  String ANNOTATION_LIBRARY = "annotationLibrary";

  /**
   * Define the default documentation Provider for CliOpts processing.
   * A NULL return value will disable the documentation provider support.
   * Override in subclasses to customize.
   * @return the default documentation provider
   */
  default DocumentationProvider defaultDocumentationProvider() {
    return null;
  }

  /**
   * Define the List of supported documentation Provider for CliOpts processing.
   * Override in subclasses to customize.
   * @return the list of supported documentation provider
   */
  default List<DocumentationProvider> supportedDocumentationProvider() {
    List<DocumentationProvider> supportedProviders = new ArrayList<>();
    supportedProviders.add(DocumentationProvider.NONE);
    return supportedProviders;
  }

  /**
   * Define the list of supported annotation libraries for CliOpts processing.
   * Override in subclasses to customize.
   * @return the list of supported annotation libraries
   */
  default List<AnnotationLibrary> supportedAnnotationLibraries() {
    List<AnnotationLibrary> supportedLibraries = new ArrayList<>();
    supportedLibraries.add(AnnotationLibrary.NONE);
    return supportedLibraries;
  }

  DocumentationProvider getDocumentationProvider();

  void setDocumentationProvider(DocumentationProvider documentationProvider);

  AnnotationLibrary getAnnotationLibrary();

  void setAnnotationLibrary(AnnotationLibrary annotationLibrary);

  enum DocumentationProvider {
      NONE("withoutDocumentationProvider", "Do not publish an OpenAPI specification.",
          AnnotationLibrary.NONE, AnnotationLibrary.values()),

      SOURCE("sourceDocumentationProvider", "Publish the original input OpenAPI specification.",
          AnnotationLibrary.NONE, AnnotationLibrary.values()),

      SWAGGER1("swagger1DocumentationProvider", "Generate an OpenAPI 2 (fka Swagger RESTful API Documentation Specification) specification using Swagger-Core 1.x.",
          AnnotationLibrary.SWAGGER1, AnnotationLibrary.SWAGGER1),

      SWAGGER2("swagger2DocumentationProvider", "Generate an OpenAPI 3 specification using Swagger-Core 2.x.",
          AnnotationLibrary.SWAGGER2, AnnotationLibrary.SWAGGER2),

      SPRINGFOX("springFoxDocumentationProvider", "Generate an OpenAPI 2 (fka Swagger RESTful API Documentation Specification) specification using SpringFox 2.x. Deprecated (for removal); use springdoc instead.",
          AnnotationLibrary.SWAGGER1, AnnotationLibrary.SWAGGER1),

      SPRINGDOC("springDocDocumentationProvider", "Generate an OpenAPI 3 specification using SpringDoc.",
          AnnotationLibrary.SWAGGER2, AnnotationLibrary.SWAGGER2);

      private final String propertyName;

      private final String description;

      private final AnnotationLibrary preferredAnnotationLibrary;

      private final AnnotationLibrary[] supportedAnnotationLibraries;

      DocumentationProvider(String propertyName, String description,
          AnnotationLibrary preferredAnnotationLibrary,
          AnnotationLibrary... supportedAnnotationLibraries) {
          this.propertyName = propertyName;
          this.description = description;
          this.preferredAnnotationLibrary = preferredAnnotationLibrary;
          this.supportedAnnotationLibraries = supportedAnnotationLibraries;
      }

      public static DocumentationProvider ofCliOption(String optVal) {
          optVal = Objects.requireNonNull(optVal).toUpperCase(Locale.ROOT);
          return valueOf(optVal);
      }

    /**
     * The property name should be used in the codegen model as a boolean property.
     *
     * @return the property name for this documentation provider
     */
      public String getPropertyName() {
          return propertyName;
      }

      public String getDescription() {
          return description;
      }

      public AnnotationLibrary getPreferredAnnotationLibrary() {
          return preferredAnnotationLibrary;
      }

      public AnnotationLibrary[] getSupportedAnnotationLibraries() {
          return supportedAnnotationLibraries;
      }

      public List<AnnotationLibrary> supportedAnnotationLibraries() {
          return Arrays.asList(getSupportedAnnotationLibraries());
      }

      public String toCliOptValue() {
          return name().toLowerCase(Locale.ROOT);
      }
  }

  enum AnnotationLibrary {
      NONE("withoutAnnotationLibrary", "Do not annotate Model and Api with complementary annotations."),

      SWAGGER1("swagger1AnnotationLibrary", "Annotate Model and Api using the Swagger Annotations 1.x library."),

      SWAGGER2("swagger2AnnotationLibrary", "Annotate Model and Api using the Swagger Annotations 2.x library."),

      MICROPROFILE("microprofileAnnotationLibrary", "Annotate Model and Api using the Microprofile annotations.");

      private final String propertyName;

      private final String description;

      public static AnnotationLibrary ofCliOption(String optVal) {
          optVal = Objects.requireNonNull(optVal).toUpperCase(Locale.ROOT);
          return valueOf(optVal);
      }

    /**
     * The property name is used in the codegen model as a boolean property.
     *
     * @return the property name for this annotation library
     */
    public String getPropertyName() {
          return propertyName;
      }

      public String getDescription() {
          return description;
      }

      AnnotationLibrary(String propertyName, String description) {
          this.propertyName = propertyName;
          this.description = description;
      }

      public String toCliOptValue() {
          return name().toLowerCase(Locale.ROOT);
      }
  }
}
