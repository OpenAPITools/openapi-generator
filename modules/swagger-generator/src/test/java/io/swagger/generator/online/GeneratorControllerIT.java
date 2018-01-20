package io.swagger.generator.online;

import com.fasterxml.jackson.databind.JsonNode;
import io.swagger.v3.core.util.Json;
import org.apache.commons.io.IOUtils;
import org.apache.http.HttpResponse;
import org.apache.http.client.HttpClient;
import org.apache.http.client.methods.HttpGet;
import org.apache.http.impl.client.HttpClientBuilder;
import org.testng.Assert;
import org.testng.annotations.Test;

public class GeneratorControllerIT {

    static final String DEFAULT_HOST = "http://localhost:8080/v2";
    private HttpClient client = HttpClientBuilder.create().build();

    @Test
    public void getClients() throws Exception {
        final HttpResponse response = client.execute(new HttpGet(DEFAULT_HOST + "/clients"));
        int responseCode = response.getStatusLine().getStatusCode();
        Assert.assertEquals(responseCode, 200);

        String json = IOUtils.toString(response.getEntity().getContent());
        JsonNode jsonNode = Json.mapper().readTree(json);
        Assert.assertTrue(jsonNode.isArray());
        Assert.assertTrue(jsonNode.toString().contains("java"));
    }

    @Test
    public void getServers() throws Exception {
        final HttpResponse response = client.execute(new HttpGet(DEFAULT_HOST + "/servers"));
        int responseCode = response.getStatusLine().getStatusCode();
        Assert.assertEquals(responseCode, 200);

        String json = IOUtils.toString(response.getEntity().getContent());
        JsonNode jsonNode = Json.mapper().readTree(json);
        Assert.assertTrue(jsonNode.isArray());
        Assert.assertTrue(jsonNode.toString().contains("inflector"));
    }
}
