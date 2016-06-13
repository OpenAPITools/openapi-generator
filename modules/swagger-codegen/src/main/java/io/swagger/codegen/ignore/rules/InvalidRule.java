package io.swagger.codegen.ignore.rules;

import java.util.List;

public class InvalidRule extends Rule {
    private final String reason;

    InvalidRule(List<Part> syntax, String definition, String reason) {
        super(syntax, definition);
        this.reason = reason;
    }

    @Override
    public Boolean matches(String relativePath) {
        return null;
    }

    @Override
    public Operation evaluate(String relativePath) {
        return Operation.NOOP;
    }

    public String getReason() {
        return reason;
    }
}
