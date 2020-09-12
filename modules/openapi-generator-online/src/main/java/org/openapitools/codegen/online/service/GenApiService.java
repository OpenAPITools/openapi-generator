/*
 * Copyright 2018 OpenAPI-Generator Contributors (https://openapi-generator.tech)
 * Copyright 2018 SmartBear Software
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     https://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.openapitools.codegen.online.service;

import org.apache.commons.io.FileUtils;
import org.apache.commons.lang3.StringUtils;
import org.openapitools.codegen.CliOption;
import org.openapitools.codegen.CodegenConfig;
import org.openapitools.codegen.CodegenConfigLoader;
import org.openapitools.codegen.CodegenType;
import org.openapitools.codegen.online.api.GenApiDelegate;
import org.openapitools.codegen.online.model.Generated;
import org.openapitools.codegen.online.model.GeneratorInput;
import org.openapitools.codegen.online.model.ResponseCode;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.core.io.ByteArrayResource;
import org.springframework.core.io.Resource;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;
import org.springframework.web.context.request.NativeWebRequest;
import org.springframework.web.server.ResponseStatusException;
import org.springframework.web.servlet.support.ServletUriComponentsBuilder;
import org.springframework.web.util.UriComponentsBuilder;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.*;

@Service
public class GenApiService implements GenApiDelegate {

    private static List<String> clients = new ArrayList<>();
    private static List<String> servers = new ArrayList<>();
    private static Map<String, Generated> fileMap = new HashMap<>();

    static {
        List<CodegenConfig> extensions = CodegenConfigLoader.getAll();
        for (CodegenConfig config : extensions) {
            if (config.getTag().equals(CodegenType.CLIENT)
                    || config.getTag().equals(CodegenType.DOCUMENTATION)) {
                clients.add(config.getName());
            } else if (config.getTag().equals(CodegenType.SERVER)) {
                servers.add(config.getName());
            }
        }

        clients.sort(String.CASE_INSENSITIVE_ORDER);
        servers.sort(String.CASE_INSENSITIVE_ORDER);
    }

    @Autowired
    private NativeWebRequest request;

    @Override
    public Optional<NativeWebRequest> getRequest() {
        return Optional.ofNullable(request);
    }

    @Override
    public ResponseEntity<Resource> downloadFile(String fileId) {
        Generated g = fileMap.get(fileId);
        System.out.println("looking for fileId " + fileId);
        System.out.println("got filename " + g.getFilename());

        File file = new File(g.getFilename());
        Path path = Paths.get(file.getAbsolutePath());
        ByteArrayResource resource;
        try {
            resource = new ByteArrayResource(Files.readAllBytes(path));
        } catch (FileNotFoundException e) {
            throw new ResponseStatusException(HttpStatus.NOT_FOUND, "File not found", e);
        } catch (IOException e) {
            throw new ResponseStatusException(HttpStatus.INTERNAL_SERVER_ERROR, "I/O error while reading file", e);
        }
        try {
            FileUtils.deleteDirectory(file.getParentFile());
        } catch (IOException e) {
            System.out.println("failed to delete file " + file.getAbsolutePath());
        }
        return ResponseEntity
                .ok()
                .contentType(MediaType.valueOf("application/zip"))
                .header("Content-Disposition",
                        "attachment; filename=\"" + g.getFriendlyName() + "-generated.zip\"")
                .header("Accept-Range", "bytes")
                //.header("Content-Length", bytes.length)
                .body(resource);
    }

    @Override
    public ResponseEntity<ResponseCode> generateClient(String language, GeneratorInput generatorInput) {
        String filename = Generator.generateClient(language, generatorInput);
        return getResponse(filename, language + "-client");
    }

    @Override
    public ResponseEntity<Map<String, CliOption>> getClientOptions(String language) {
        Map<String, CliOption> opts = Generator.getOptions(language);

        if (opts != null) {
            return ResponseEntity.ok().body(opts);
        } else {
            return ResponseEntity.notFound().build();
        }
    }

    @Override
    public ResponseEntity<Map<String, CliOption>> getServerOptions(String framework) {
        Map<String, CliOption> opts = Generator.getOptions(framework);

        if (opts != null) {
            return ResponseEntity.ok().body(opts);
        } else {
            return ResponseEntity.notFound().build();
        }
    }

    @Override
    public ResponseEntity<List<String>> clientOptions() {
        return ResponseEntity.ok().body(clients);
    }

    @Override
    public ResponseEntity<List<String>> serverOptions() {
        return ResponseEntity.ok().body(servers);
    }

    @Override
    public ResponseEntity<ResponseCode> generateServerForLanguage(String framework, GeneratorInput generatorInput) {
        if (framework == null) {
            throw new ResponseStatusException(HttpStatus.BAD_REQUEST, "Framework is required");
        }
        String filename = Generator.generateServer(framework, generatorInput);
        System.out.println("generated name: " + filename);

        return getResponse(filename, framework + "-server");
    }

    private ResponseEntity<ResponseCode> getResponse(String filename, String friendlyName) {
        String host = System.getenv("GENERATOR_HOST");

        UriComponentsBuilder uriBuilder;
        if (!StringUtils.isBlank(host)) {
            uriBuilder = UriComponentsBuilder.fromUriString(host);
        } else {
            uriBuilder = ServletUriComponentsBuilder.fromCurrentContextPath();
        }

        if (filename != null) {
            String code = UUID.randomUUID().toString();
            Generated g = new Generated();
            g.setFilename(filename);
            g.setFriendlyName(friendlyName);
            fileMap.put(code, g);
            System.out.println(code + ", " + filename);
            String link = uriBuilder.path("/api/gen/download/").path(code).toUriString();
            return ResponseEntity.ok().body(new ResponseCode(code, link));
        } else {
            return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR).build();
        }
    }

}
