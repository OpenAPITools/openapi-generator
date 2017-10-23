package io.swagger.codegen.cmd;

import io.swagger.parser.v3.OpenAPIV3Parser;

import java.util.HashSet;
import java.util.List;
import java.util.Set;

public class Validate implements Runnable {

    private String spec;

    public void setSpec(String spec) {
        this.spec = spec;
    }

    @Override
    public void run() {
        System.out.println("Validating spec file (" + spec + ")");

        OpenAPIV3Parser parser = new OpenAPIV3Parser();;
        List<String> messageList = parser.readWithInfo(spec, null).getMessages();
        Set<String> messages = new HashSet<String>(messageList);

        for (String message : messages) {
            System.out.println(message);
        }

        if (messages.size() > 0) {
            throw new ValidateException();
        }
    }
}
