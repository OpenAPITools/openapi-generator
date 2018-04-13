package io.swagger.codegen.languages;

import static io.swagger.codegen.languages.JavaClientCodegen.RETROFIT_2;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.testng.Assert;
import org.testng.annotations.Test;

import com.google.common.collect.ImmutableMap;

import io.swagger.codegen.CodegenConstants;
import io.swagger.codegen.CodegenOperation;
import io.swagger.codegen.CodegenParameter;

public class JavaClientCodegenTest {

    private static final String VENDOR_MIME_TYPE = "application/vnd.company.v1+json";
    private static final String XML_MIME_TYPE = "application/xml";
    private static final String JSON_MIME_TYPE = "application/json";
    private static final String TEXT_MIME_TYPE = "text/plain";

    @Test
    public void testJsonMime() {
        Assert.assertTrue(JavaClientCodegen.isJsonMimeType(JSON_MIME_TYPE));
        Assert.assertFalse(JavaClientCodegen.isJsonMimeType(XML_MIME_TYPE));
        Assert.assertFalse(JavaClientCodegen.isJsonMimeType(TEXT_MIME_TYPE));

        Assert.assertTrue(JavaClientCodegen.isJsonVendorMimeType("application/vnd.mycompany+json"));
        Assert.assertTrue(JavaClientCodegen.isJsonVendorMimeType("application/vnd.mycompany.v1+json"));
        Assert.assertTrue(JavaClientCodegen.isJsonVendorMimeType("application/vnd.mycompany.resourceTypeA.version1+json"));
        Assert.assertTrue(JavaClientCodegen.isJsonVendorMimeType("application/vnd.mycompany.resourceTypeB.version2+json"));    
        Assert.assertFalse(JavaClientCodegen.isJsonVendorMimeType("application/v.json"));

    }

    @Test
    public void testContentTypePrioritization() {
        Map<String, String> jsonMimeType = new HashMap<>();
        jsonMimeType.put(JavaClientCodegen.MEDIA_TYPE, JSON_MIME_TYPE);

        Map<String, String> xmlMimeType = new HashMap<>();
        xmlMimeType.put(JavaClientCodegen.MEDIA_TYPE, XML_MIME_TYPE);

        Map<String, String> vendorMimeType = new HashMap<>();
        vendorMimeType.put(JavaClientCodegen.MEDIA_TYPE, VENDOR_MIME_TYPE);

        Map<String, String> textMimeType = new HashMap<>();
        textMimeType.put(JavaClientCodegen.MEDIA_TYPE, TEXT_MIME_TYPE);

        Assert.assertEquals(JavaClientCodegen.prioritizeContentTypes(
                Collections.<Map<String,String>>emptyList()), Collections.emptyList());

        Assert.assertEquals(JavaClientCodegen.prioritizeContentTypes(Arrays.asList(xmlMimeType)), Arrays.asList(xmlMimeType));
        Assert.assertEquals(JavaClientCodegen.prioritizeContentTypes(Arrays.asList(jsonMimeType)), Arrays.asList(jsonMimeType));
        Assert.assertEquals(JavaClientCodegen.prioritizeContentTypes(Arrays.asList(vendorMimeType)), Arrays.asList(vendorMimeType));

        Assert.assertEquals(JavaClientCodegen.prioritizeContentTypes(Arrays.asList(xmlMimeType, jsonMimeType)),
                Arrays.asList(jsonMimeType, xmlMimeType));
        Assert.assertEquals(JavaClientCodegen.prioritizeContentTypes(Arrays.asList(jsonMimeType, xmlMimeType)),
                Arrays.asList(jsonMimeType, xmlMimeType));
        Assert.assertEquals(JavaClientCodegen.prioritizeContentTypes(Arrays.asList(jsonMimeType, vendorMimeType)),
                Arrays.asList(vendorMimeType, jsonMimeType));
        Assert.assertEquals(JavaClientCodegen.prioritizeContentTypes(Arrays.asList(textMimeType, xmlMimeType)),
                Arrays.asList(textMimeType, xmlMimeType));
        Assert.assertEquals(JavaClientCodegen.prioritizeContentTypes(Arrays.asList(xmlMimeType, textMimeType)),
                Arrays.asList(xmlMimeType, textMimeType));

        System.out.println(JavaClientCodegen.prioritizeContentTypes(Arrays.asList(
                xmlMimeType,textMimeType, jsonMimeType, vendorMimeType)));

        List<Map<String,String>> priContentTypes = JavaClientCodegen.prioritizeContentTypes(Arrays.asList(
                xmlMimeType, textMimeType, jsonMimeType, vendorMimeType));
        Assert.assertEquals(priContentTypes, Arrays.asList(vendorMimeType, jsonMimeType, xmlMimeType, textMimeType));
  
        for ( int i = 0; i < 3; i++ )
            Assert.assertNotNull(priContentTypes.get(i).get("hasMore"));
 
        Assert.assertNull(priContentTypes.get(3).get("hasMore"));
    }

    @Test
    public void testParametersAreCorrectlyOrderedWhenUsingRetrofit(){
        JavaClientCodegen javaClientCodegen = new JavaClientCodegen();
        javaClientCodegen.setLibrary(RETROFIT_2);

        CodegenOperation codegenOperation = new CodegenOperation();
        CodegenParameter queryParamRequired = createQueryParam("queryParam1", true);
        CodegenParameter queryParamOptional = createQueryParam("queryParam2", false);
        CodegenParameter pathParam1 = createPathParam("pathParam1");
        CodegenParameter pathParam2 = createPathParam("pathParam2");

        codegenOperation.allParams = Arrays.asList(queryParamRequired, pathParam1, pathParam2, queryParamOptional);
        Map<String, Object> operations = ImmutableMap.<String, Object>of("operation", Arrays.asList(codegenOperation));

        Map<String, Object> objs = ImmutableMap.of("operations", operations, "imports", new ArrayList<Map<String, String>>());

        javaClientCodegen.postProcessOperations(objs);

        Assert.assertEquals(Arrays.asList(pathParam1, pathParam2, queryParamRequired, queryParamOptional), codegenOperation.allParams);
        Assert.assertTrue(pathParam1.hasMore);
        Assert.assertTrue(pathParam2.hasMore);
        Assert.assertTrue(queryParamRequired.hasMore);
        Assert.assertFalse(queryParamOptional.hasMore);

    }
    
    @Test
    public void testInitialConfigValues() throws Exception {
        final JavaClientCodegen codegen = new JavaClientCodegen();
        codegen.processOpts();

        Assert.assertEquals(codegen.additionalProperties().get(CodegenConstants.HIDE_GENERATION_TIMESTAMP), Boolean.FALSE);
        Assert.assertEquals(codegen.isHideGenerationTimestamp(), false);

        Assert.assertEquals(codegen.modelPackage(), "io.swagger.client.model");
        Assert.assertEquals(codegen.additionalProperties().get(CodegenConstants.MODEL_PACKAGE), "io.swagger.client.model");
        Assert.assertEquals(codegen.apiPackage(), "io.swagger.client.api");
        Assert.assertEquals(codegen.additionalProperties().get(CodegenConstants.API_PACKAGE), "io.swagger.client.api");
        Assert.assertEquals(codegen.invokerPackage, "io.swagger.client");
        Assert.assertEquals(codegen.additionalProperties().get(CodegenConstants.INVOKER_PACKAGE), "io.swagger.client");
    }

    @Test
    public void testSettersForConfigValues() throws Exception {
        final JavaClientCodegen codegen = new JavaClientCodegen();
        codegen.setHideGenerationTimestamp(true);
        codegen.setModelPackage("xyz.yyyyy.zzzzzzz.model");
        codegen.setApiPackage("xyz.yyyyy.zzzzzzz.api");
        codegen.setInvokerPackage("xyz.yyyyy.zzzzzzz.invoker");
        codegen.processOpts();

        Assert.assertEquals(codegen.additionalProperties().get(CodegenConstants.HIDE_GENERATION_TIMESTAMP), Boolean.TRUE);
        Assert.assertEquals(codegen.isHideGenerationTimestamp(), true);
        Assert.assertEquals(codegen.modelPackage(), "xyz.yyyyy.zzzzzzz.model");
        Assert.assertEquals(codegen.additionalProperties().get(CodegenConstants.MODEL_PACKAGE), "xyz.yyyyy.zzzzzzz.model");
        Assert.assertEquals(codegen.apiPackage(), "xyz.yyyyy.zzzzzzz.api");
        Assert.assertEquals(codegen.additionalProperties().get(CodegenConstants.API_PACKAGE), "xyz.yyyyy.zzzzzzz.api");
        Assert.assertEquals(codegen.invokerPackage, "xyz.yyyyy.zzzzzzz.invoker");
        Assert.assertEquals(codegen.additionalProperties().get(CodegenConstants.INVOKER_PACKAGE), "xyz.yyyyy.zzzzzzz.invoker");
    }

    @Test
    public void testAdditionalPropertiesPutForConfigValues() throws Exception {
        final JavaClientCodegen codegen = new JavaClientCodegen();
        codegen.additionalProperties().put(CodegenConstants.HIDE_GENERATION_TIMESTAMP, "true");
        codegen.additionalProperties().put(CodegenConstants.MODEL_PACKAGE, "xyz.yyyyy.zzzzzzz.mmmmm.model");
        codegen.additionalProperties().put(CodegenConstants.API_PACKAGE, "xyz.yyyyy.zzzzzzz.aaaaa.api");
        codegen.additionalProperties().put(CodegenConstants.INVOKER_PACKAGE,"xyz.yyyyy.zzzzzzz.iiii.invoker");
        codegen.processOpts();

        Assert.assertEquals(codegen.additionalProperties().get(CodegenConstants.HIDE_GENERATION_TIMESTAMP), Boolean.TRUE);
        Assert.assertEquals(codegen.isHideGenerationTimestamp(), true);
        Assert.assertEquals(codegen.modelPackage(), "xyz.yyyyy.zzzzzzz.mmmmm.model");
        Assert.assertEquals(codegen.additionalProperties().get(CodegenConstants.MODEL_PACKAGE), "xyz.yyyyy.zzzzzzz.mmmmm.model");
        Assert.assertEquals(codegen.apiPackage(), "xyz.yyyyy.zzzzzzz.aaaaa.api");
        Assert.assertEquals(codegen.additionalProperties().get(CodegenConstants.API_PACKAGE), "xyz.yyyyy.zzzzzzz.aaaaa.api");
        Assert.assertEquals(codegen.invokerPackage, "xyz.yyyyy.zzzzzzz.iiii.invoker");
        Assert.assertEquals(codegen.additionalProperties().get(CodegenConstants.INVOKER_PACKAGE), "xyz.yyyyy.zzzzzzz.iiii.invoker");
    }

    @Test
    public void testPackageNamesSetInvokerDerivedFromApi() throws Exception {
        final JavaClientCodegen codegen = new JavaClientCodegen();
        codegen.additionalProperties().put(CodegenConstants.MODEL_PACKAGE, "xyz.yyyyy.zzzzzzz.mmmmm.model");
        codegen.additionalProperties().put(CodegenConstants.API_PACKAGE, "xyz.yyyyy.zzzzzzz.aaaaa.api");
        codegen.processOpts();

        Assert.assertEquals(codegen.modelPackage(), "xyz.yyyyy.zzzzzzz.mmmmm.model");
        Assert.assertEquals(codegen.additionalProperties().get(CodegenConstants.MODEL_PACKAGE), "xyz.yyyyy.zzzzzzz.mmmmm.model");
        Assert.assertEquals(codegen.apiPackage(), "xyz.yyyyy.zzzzzzz.aaaaa.api");
        Assert.assertEquals(codegen.additionalProperties().get(CodegenConstants.API_PACKAGE), "xyz.yyyyy.zzzzzzz.aaaaa.api");
        Assert.assertEquals(codegen.invokerPackage, "xyz.yyyyy.zzzzzzz.aaaaa");
        Assert.assertEquals(codegen.additionalProperties().get(CodegenConstants.INVOKER_PACKAGE), "xyz.yyyyy.zzzzzzz.aaaaa");
    }

    @Test
    public void testPackageNamesSetInvokerDerivedFromModel() throws Exception {
        final JavaClientCodegen codegen = new JavaClientCodegen();
        codegen.additionalProperties().put(CodegenConstants.MODEL_PACKAGE, "xyz.yyyyy.zzzzzzz.mmmmm.model");
        codegen.processOpts();

        Assert.assertEquals(codegen.modelPackage(), "xyz.yyyyy.zzzzzzz.mmmmm.model");
        Assert.assertEquals(codegen.additionalProperties().get(CodegenConstants.MODEL_PACKAGE), "xyz.yyyyy.zzzzzzz.mmmmm.model");
        Assert.assertEquals(codegen.apiPackage(), "io.swagger.client.api");
        Assert.assertEquals(codegen.additionalProperties().get(CodegenConstants.API_PACKAGE), "io.swagger.client.api");
        Assert.assertEquals(codegen.invokerPackage, "xyz.yyyyy.zzzzzzz.mmmmm");
        Assert.assertEquals(codegen.additionalProperties().get(CodegenConstants.INVOKER_PACKAGE), "xyz.yyyyy.zzzzzzz.mmmmm");
    }

    private CodegenParameter createPathParam(String name) {
        CodegenParameter codegenParameter = createStringParam(name);
        codegenParameter.isPathParam = true;
        return codegenParameter;
    }

    private CodegenParameter createQueryParam(String name, boolean required) {
        CodegenParameter codegenParameter = createStringParam(name);
        codegenParameter.isQueryParam = true;
        codegenParameter.required = required;
        return codegenParameter;
    }

    private CodegenParameter createStringParam(String name){
        CodegenParameter codegenParameter = new CodegenParameter();
        codegenParameter.paramName = name;
        codegenParameter.baseName = name;
        codegenParameter.dataType = "String";
        return codegenParameter;
    }
}
