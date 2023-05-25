package org.openapitools.api

import groovy.json.JsonBuilder
import groovy.json.JsonGenerator
import groovyx.net.http.ChainedHttpConfig
import groovyx.net.http.ContentTypes
import groovyx.net.http.NativeHandlers
import groovyx.net.http.ToServer

import static groovyx.net.http.HttpBuilder.configure
import static java.net.URI.create

class ApiUtils {

    static def jsonGenerator = new JsonGenerator.Options()
            .addConverter(Enum) { Enum u, String key ->
                u.toString()
            }
            .build()

    void invokeApi(onSuccess, onFailure, basePath, versionPath, resourcePath, queryParams, headerParams, bodyParams, contentType, method, container, type)  {
        def (url, uriPath) = buildUrlAndUriPath(basePath, versionPath, resourcePath)
        println "url=$url uriPath=$uriPath"
        def http = configure {
            request.uri = url
            request.uri.path = uriPath
            request.encoder(ContentTypes.JSON, { final ChainedHttpConfig config, final ToServer ts ->
                final ChainedHttpConfig.ChainedRequest request = config.getChainedRequest();
                if (NativeHandlers.Encoders.handleRawUpload(config, ts)) {
                    return;
                }

                final Object body = NativeHandlers.Encoders.checkNull(request.actualBody());
                final String json = ((body instanceof String || body instanceof GString)
                        ? body.toString()
                        : new JsonBuilder(body, jsonGenerator).toString());
                ts.toServer(NativeHandlers.Encoders.stringToStream(json, request.actualCharset()));
            })
        }
        .invokeMethod(String.valueOf(method).toLowerCase()) {
            request.uri.query = queryParams
            request.headers = headerParams
            if (bodyParams != null) {
                request.body = bodyParams
            }
            request.contentType = contentType

            response.success { resp, json ->
                if (type != null) {
                    onSuccess(parse(json, container, type))
                }
            }
            response.failure { resp ->
                onFailure(resp.statusCode, resp.message)
            }
        }

    }

    private static def buildUrlAndUriPath(basePath, versionPath, resourcePath) {
        // HTTPBuilder expects to get as its constructor parameter an URL,
        // without any other additions like path, therefore we need to cut the path
        // from the basePath as it is represented by swagger APIs
        // we use java.net.URI to manipulate the basePath
        // then the uriPath will hold the rest of the path
        URI baseUri =  create(basePath)
        def pathOnly = baseUri.getPath()
        [basePath-pathOnly, pathOnly+versionPath+resourcePath]
    }

    private def parse(object, container, clazz) {
        if (container == "array") {
            return object.collect {parse(it, "", clazz)}
        }   else {
            return clazz.newInstance(object)
        }
    }

}
