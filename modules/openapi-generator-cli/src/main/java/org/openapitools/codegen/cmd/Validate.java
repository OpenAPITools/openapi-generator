package org.openapitools.codegen.cmd;

import io.airlift.airline.Command;
import io.airlift.airline.Option;

import io.swagger.parser.OpenAPIParser;
import io.swagger.v3.parser.core.models.SwaggerParseResult;

import java.util.HashSet;
import java.util.List;
import java.util.Set;

@Command(name = "validate", description = "Validate specification")
public class Validate implements Runnable {

    @Option(name = {"-i", "--input-spec"}, title = "spec file", required = true,
            description = "location of the OpenAPI spec, as URL or file (required)")
    private String spec;

    @Override
    public void run() {
        System.out.println("Validating spec file (" + spec + ")");

        SwaggerParseResult result = new OpenAPIParser().readLocation(spec, null, null);
        List<String> messageList = result.getMessages();
        Set<String> messages = new HashSet<String>(messageList);

        for (String message : messages) {
            System.out.println(message);
        }

        if (messages.size() > 0) {
            throw new ValidateException();
        }
    }
}
