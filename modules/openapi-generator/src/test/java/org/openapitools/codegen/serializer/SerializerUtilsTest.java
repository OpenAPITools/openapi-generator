package org.openapitools.codegen.serializer;

import io.swagger.v3.oas.models.*;
import io.swagger.v3.oas.models.info.Info;
import io.swagger.v3.oas.models.media.ObjectSchema;
import io.swagger.v3.oas.models.media.StringSchema;
import io.swagger.v3.oas.models.responses.ApiResponse;
import io.swagger.v3.oas.models.responses.ApiResponses;
import io.swagger.v3.oas.models.security.SecurityRequirement;
import io.swagger.v3.oas.models.servers.Server;
import io.swagger.v3.oas.models.tags.Tag;
import org.testng.annotations.Test;

import java.util.Arrays;
import java.util.LinkedHashMap;

import static org.testng.Assert.assertEquals;

public class SerializerUtilsTest {

    @Test
    public void testToYamlStringCompleteExample() throws Exception {
        OpenAPI openAPI = createCompleteExample();

        String content = SerializerUtils.toYamlString(openAPI);
        String expected = "openapi: 3.0.1\n" +
               "info:\n" +
               "  description: Some description\n" +
               "  title: Some title\n" +
               "externalDocs:\n" +
               "  description: a-description\n" +
               "  url: http://abcdef.com\n" +
               "servers:\n" +
               "- description: first server\n" +
               "  url: http://www.server1.com\n" +
               "- description: second server\n" +
               "  url: http://www.server2.com\n" +
               "security:\n" +
               "- some_auth:\n" +
               "  - write\n" +
               "  - read\n" +
               "tags:\n" +
               "- description: some 1 description\n" +
               "  name: tag1\n" +
               "- description: some 2 description\n" +
               "  name: tag2\n" +
               "- description: some 3 description\n" +
               "  name: tag3\n" +
               "paths:\n" +
               "  /ping/pong:\n" +
               "    get:\n" +
               "      description: Some description\n" +
               "      operationId: pingOp\n" +
               "      responses:\n" +
               "        \"200\":\n" +
               "          description: Ok\n" +
               "components:\n" +
               "  schemas:\n" +
               "    SomeObject:\n" +
               "      description: An Obj\n" +
               "      properties:\n" +
               "        id:\n" +
               "          type: string\n" +
               "      type: object\n" +
               "x-custom: value1\n" +
               "x-other: value2\n";
        assertEquals(content, expected);
    }

    @Test
    public void testToJsonStringCompleteExample() throws Exception {
        OpenAPI openAPI = createCompleteExample();

        String content = SerializerUtils.toJsonString(openAPI);
        String expected = "" +
                "{\n" +
                "  \"openapi\" : \"3.0.1\",\n" +
                "  \"info\" : {\n" +
                "    \"description\" : \"Some description\",\n" +
                "    \"title\" : \"Some title\"\n" +
                "  },\n" +
                "  \"externalDocs\" : {\n" +
                "    \"description\" : \"a-description\",\n" +
                "    \"url\" : \"http://abcdef.com\"\n" +
                "  },\n" +
                "  \"servers\" : [ {\n" +
                "    \"description\" : \"first server\",\n" +
                "    \"url\" : \"http://www.server1.com\"\n" +
                "  }, {\n" +
                "    \"description\" : \"second server\",\n" +
                "    \"url\" : \"http://www.server2.com\"\n" +
                "  } ],\n" +
                "  \"security\" : [ {\n" +
                "    \"some_auth\" : [ \"write\", \"read\" ]\n" +
                "  } ],\n" +
                "  \"tags\" : [ {\n" +
                "    \"description\" : \"some 1 description\",\n" +
                "    \"name\" : \"tag1\"\n" +
                "  }, {\n" +
                "    \"description\" : \"some 2 description\",\n" +
                "    \"name\" : \"tag2\"\n" +
                "  }, {\n" +
                "    \"description\" : \"some 3 description\",\n" +
                "    \"name\" : \"tag3\"\n" +
                "  } ],\n" +
                "  \"paths\" : {\n" +
                "    \"/ping/pong\" : {\n" +
                "      \"get\" : {\n" +
                "        \"description\" : \"Some description\",\n" +
                "        \"operationId\" : \"pingOp\",\n" +
                "        \"responses\" : {\n" +
                "          \"200\" : {\n" +
                "            \"description\" : \"Ok\"\n" +
                "          }\n" +
                "        }\n" +
                "      }\n" +
                "    }\n" +
                "  },\n" +
                "  \"components\" : {\n" +
                "    \"schemas\" : {\n" +
                "      \"SomeObject\" : {\n" +
                "        \"description\" : \"An Obj\",\n" +
                "        \"properties\" : {\n" +
                "          \"id\" : {\n" +
                "            \"type\" : \"string\"\n" +
                "          }\n" +
                "        },\n" +
                "        \"type\" : \"object\"\n" +
                "      }\n" +
                "    }\n" +
                "  },\n" +
                "  \"x-custom\" : \"value1\",\n" +
                "  \"x-other\" : \"value2\"\n" +
                "}";
        assertEquals(content, expected);
    }

    private OpenAPI createCompleteExample() {
        OpenAPI openAPI = new OpenAPI();
        openAPI.setInfo(new Info().title("Some title").description("Some description"));
        openAPI.setExternalDocs(new ExternalDocumentation().url("http://abcdef.com").description("a-description"));
        openAPI.setServers(Arrays.asList(
                new Server().url("http://www.server1.com").description("first server"),
                new Server().url("http://www.server2.com").description("second server")
            ));
        openAPI.setSecurity(Arrays.asList(
                new SecurityRequirement().addList("some_auth", Arrays.asList("write", "read"))
            ));
        openAPI.setTags(Arrays.asList(
                new Tag().name("tag1").description("some 1 description"),
                new Tag().name("tag2").description("some 2 description"),
                new Tag().name("tag3").description("some 3 description")
            ));
        openAPI.path("/ping/pong", new PathItem().get(new Operation()
                .description("Some description")
                .operationId("pingOp")
                .responses(new ApiResponses().addApiResponse("200", new ApiResponse().description("Ok")))));
        openAPI.components(new Components().addSchemas("SomeObject", new ObjectSchema().description("An Obj").addProperties("id", new StringSchema())));
        openAPI.setExtensions(new LinkedHashMap<>()); // required because swagger-core is using HashMap instead of LinkedHashMap internally.
        openAPI.addExtension("x-custom", "value1");
        openAPI.addExtension("x-other", "value2");
        return openAPI;
    }

    @Test
    public void testToYamlStringMinimalExample() throws Exception {
        OpenAPI openAPI = createMinimalExample();

        String content = SerializerUtils.toYamlString(openAPI);
        String expected = "openapi: 3.0.1\n" +
                "info:\n" +
                "  title: Some title\n" +
                "servers:\n" +
                "- url: http://www.server1.com\n" +
                "paths:\n" +
                "  /ping/pong:\n" +
                "    get:\n" +
                "      description: Some description\n" +
                "      operationId: pingOp\n" +
                "      responses:\n" +
                "        \"200\":\n" +
                "          description: Ok\n"; 
        assertEquals(content, expected);
    }

    @Test
    public void testToJsonStringMinimalExample() throws Exception {
        OpenAPI openAPI = createMinimalExample();

        String content = SerializerUtils.toJsonString(openAPI);
        String expected = "" +
                "{\n" +
                "  \"openapi\" : \"3.0.1\",\n" +
                "  \"info\" : {\n" +
                "    \"title\" : \"Some title\"\n" +
                "  },\n" +
                "  \"servers\" : [ {\n" +
                "    \"url\" : \"http://www.server1.com\"\n" +
                "  } ],\n" +
                "  \"paths\" : {\n" +
                "    \"/ping/pong\" : {\n" +
                "      \"get\" : {\n" +
                "        \"description\" : \"Some description\",\n" +
                "        \"operationId\" : \"pingOp\",\n" +
                "        \"responses\" : {\n" +
                "          \"200\" : {\n" +
                "            \"description\" : \"Ok\"\n" +
                "          }\n" +
                "        }\n" +
                "      }\n" +
                "    }\n" +
                "  }\n" +
                "}"; 
        assertEquals(content, expected);
    }

    private OpenAPI createMinimalExample() {
        OpenAPI openAPI = new OpenAPI();
        openAPI.setInfo(new Info().title("Some title"));
        openAPI.setServers(Arrays.asList(
                new Server().url("http://www.server1.com")
            ));
        openAPI.path("/ping/pong", new PathItem().get(new Operation()
                .description("Some description")
                .operationId("pingOp")
                .responses(new ApiResponses().addApiResponse("200", new ApiResponse().description("Ok")))));
        return openAPI;
    }
}
