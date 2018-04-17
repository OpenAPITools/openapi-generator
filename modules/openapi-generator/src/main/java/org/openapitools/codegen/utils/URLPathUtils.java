package org.openapitools.codegen.utils;

import org.openapitools.codegen.CodegenConfig;
import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.oas.models.servers.Server;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.net.MalformedURLException;
import java.net.URL;
import java.util.List;

public class URLPathUtils {

    protected static final Logger LOGGER = LoggerFactory.getLogger(URLPathUtils.class);
    public static String DEFAULT_PATH = "/";
    public static final String LOCAL_HOST = "http://localhost";

    public static URL getServerURL(OpenAPI openAPI) {
        final List<Server> servers = openAPI.getServers();
        if (servers == null || servers.isEmpty()) {
            return null;
        }
        // TOOD need a way to obtain all server URLs
        final Server server = servers.get(0);
        String url = server.getUrl();
        if (url.equals(DEFAULT_PATH)) {
            url = LOCAL_HOST;
        }
        try {
            return new URL(server.getUrl());
        } catch (MalformedURLException e) {
            LOGGER.warn("Not valid URL: " + server.getUrl(), e);
            return null;
        }
    }

    public static String getScheme(OpenAPI openAPI, CodegenConfig config) {
        String scheme;
        URL url = getServerURL(openAPI);
        if (url != null) {
            scheme = url.getProtocol();
        } else {
            scheme = "https";
        }
        if (config != null) {
            scheme = config.escapeText(scheme);
        }
        return scheme;
    }

    public static String getHost(OpenAPI openAPI){
        if (openAPI.getServers() != null && openAPI.getServers().size() > 0) {
            return openAPI.getServers().get(0).getUrl();
        }
        return LOCAL_HOST;
    }

}