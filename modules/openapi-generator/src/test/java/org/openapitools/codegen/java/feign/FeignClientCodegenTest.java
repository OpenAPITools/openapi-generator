package org.openapitools.codegen.java.feign;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.fail;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Optional;
import java.util.stream.Collectors;

import org.openapitools.codegen.ClientOptInput;
import org.openapitools.codegen.MockDefaultGenerator;
import org.openapitools.codegen.TestUtils;
import org.openapitools.codegen.config.CodegenConfigurator;
import org.openapitools.codegen.languages.JavaClientCodegen;
import org.openapitools.codegen.languages.JavaJAXRSSpecServerCodegen;
import org.testng.annotations.Test;

import com.github.javaparser.StaticJavaParser;
import com.github.javaparser.ast.CompilationUnit;
import com.github.javaparser.ast.body.BodyDeclaration;
import com.github.javaparser.ast.body.ClassOrInterfaceDeclaration;
import com.github.javaparser.ast.body.MethodDeclaration;
import com.github.javaparser.ast.body.Parameter;
import com.github.javaparser.ast.expr.AnnotationExpr;
import com.github.javaparser.ast.expr.SingleMemberAnnotationExpr;
import com.github.javaparser.ast.expr.StringLiteralExpr;
import com.github.javaparser.ast.type.ClassOrInterfaceType;

public class FeignClientCodegenTest {

	@Test
	public void handleSpecialCharacters() throws IOException {
        String feignClientCode = generatedFeignClientCodeAsString();
		CompilationUnit compilation = StaticJavaParser.parse(feignClientCode);
        validate(compilation);
	}

	private void validate(CompilationUnit compilation) {
		compilation.getChildNodes().stream()
			.filter(node -> node.getClass().equals(ClassOrInterfaceDeclaration.class))
        	.findFirst().map(node -> (ClassOrInterfaceDeclaration) node).orElseThrow(() -> new IllegalStateException("ClassOrInterfaceDeclaration not found."))
        	.getMembers()
        	.forEach(this::validateMember);
	}

	private void validateMember(BodyDeclaration<?> node) {
		if(node instanceof MethodDeclaration) {
			validate((MethodDeclaration) node);
		}
		else if ( node instanceof ClassOrInterfaceDeclaration) {
			validate((ClassOrInterfaceDeclaration) node);
		}
		else {
			fail(String.format("Unexpected node %s.", node));
		}
	}

	private void validate(MethodDeclaration methodDeclaration) {
		AnnotationExpr headerAnnotation = methodDeclaration.getAnnotations().stream()
			.filter(annotation -> "Headers".equals(annotation.getNameAsString()))
			.findFirst().orElseThrow(() -> new IllegalStateException("Headers-annotation not found"));
		List<String> headers = headerAnnotation.getChildNodes().stream()
			.flatMap(node -> node.getChildNodes().stream())
			.filter(node -> node.getClass().equals(StringLiteralExpr.class))
			.map(node -> (StringLiteralExpr) node)
			.map(node -> node.getValue())
			.collect(Collectors.toList());
		assertThat(headers).containsExactlyInAnyOrder("Content-Type: application/json", "Accept: application/json",
				"X-Correlation-ID: {X-Correlation-ID}", "header_with_underscore: {header_with_underscore}");

		assertThat(methodDeclaration.getNameAsString()).isEqualTo("postExample");

		validateParamAnnotation(methodDeclaration, "xCorrelationID", "X-Correlation-ID");
		validateParamAnnotation(methodDeclaration, "headerWithUnderscore", "header_with_underscore");
		assertThat(methodDeclaration.getParameterByName("body")).isNotNull();
		Optional<Parameter> queryParam = methodDeclaration.getParameterByName("queryParams");
		if(queryParam.isPresent()) {
			assertThat(queryParam.get().getAnnotationByName("QueryMap")).isNotNull();
		}
		else {
			validateParamAnnotation(methodDeclaration, "queryParamWithDash", "query-param-with-dash");
			validateParamAnnotation(methodDeclaration, "queryParamWithUnderscore", "query_param_with_underscore");
		}
	}

	private void validateParamAnnotation(MethodDeclaration methodDeclaration, String parameterName, String expectedParamAnnotationValue) {
		Parameter parameter = methodDeclaration.getParameterByName(parameterName)
			.orElseThrow(() -> new IllegalStateException(String.format("Parameter %s not found", parameterName)));
		SingleMemberAnnotationExpr annotation = (SingleMemberAnnotationExpr) parameter.getAnnotationByName("Param")
			.orElseThrow(() -> new IllegalStateException(String.format("Annotation 'Param' not found on parameter %s", parameterName)));
		assertThat(annotation.getMemberValue().asStringLiteralExpr().asString()).isEqualTo(expectedParamAnnotationValue);
	}

	private void validate(ClassOrInterfaceDeclaration classOrInterfaceDeclaration) {
		String parentClassName = classOrInterfaceDeclaration.getChildNodes().stream()
			.filter(node -> node.getClass().equals(ClassOrInterfaceType.class))
			.findFirst().map(node -> (ClassOrInterfaceType) node).orElseThrow(() -> new IllegalStateException("HashMap parent-class not found."))
			.getNameAsString();
		assertThat(parentClassName).isEqualTo(HashMap.class.getSimpleName());
	}

	private String generatedFeignClientCodeAsString() throws IOException {
		File output = Files.createTempDirectory("test").toFile();
        output.deleteOnExit();

        Map<String, Object> properties = new HashMap<>();
        properties.put(JavaClientCodegen.JAVA8_MODE, true);
        properties.put(JavaJAXRSSpecServerCodegen.OPEN_API_SPEC_FILE_LOCATION, "openapi.yml");

        final CodegenConfigurator configurator = new CodegenConfigurator()
                .setGeneratorName("java")
                .setLibrary("feign")
                .setAdditionalProperties(properties)
                .setInputSpec("src/test/resources/3_0/special-characters.yaml")
                .setOutputDir(output.getAbsolutePath().replace("\\", "/"));

        final ClientOptInput clientOptInput = configurator.toClientOptInput();
        MockDefaultGenerator generator = new MockDefaultGenerator();
        generator.opts(clientOptInput).generate();

        Map<String, String> generatedFiles = generator.getFiles();
        TestUtils.validateJavaSourceFiles(generatedFiles);
        Optional<Entry<String, String>> exampleApiContentEntryOptional = generatedFiles.entrySet().stream()
        	.filter(entry -> entry.getKey().endsWith("org/openapitools/client/api/ExampleApi.java"))
        	.findFirst();
        if(!exampleApiContentEntryOptional.isPresent() ) {
        	fail("ExampleApi not found");
        }
		return exampleApiContentEntryOptional.get().getValue();
	}
}
