package io.swagger.codegen.cmd;

import io.airlift.airline.Command;
import io.airlift.airline.Option;

import io.swagger.parser.SwaggerParser;
import io.swagger.parser.util.SwaggerDeserializationResult;

import java.util.HashSet;
import java.util.List;
import java.util.Set;

@Command(name = "validate", description = "Validate specification")
public class Validate implements Runnable {

    @Option(name = {"-i", "--input-spec"}, title = "spec file", required = true,
            description = "location of the swagger spec, as URL or file (required)")
    private String spec;

    @Override
    public void run() {
        System.out.println("Validating spec file (" + spec + ")");

        SwaggerParser parser = new SwaggerParser();
        SwaggerDeserializationResult result = parser.readWithInfo(spec, null, true);
        List<String> messageList = result.getMessages();
        Set<String> messages = new HashSet<String>(messageList);

        for (String message: messages) {
            System.out.println(message);
        }

        if (messages.size() > 0) {
            throw new ValidateException();
        }
    }
}