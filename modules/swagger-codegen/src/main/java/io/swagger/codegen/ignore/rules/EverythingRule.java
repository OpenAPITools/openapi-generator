package io.swagger.codegen.ignore.rules;

import java.util.List;

/**
 * An ignore rule which matches everything.
 */
public class EverythingRule extends Rule {
    EverythingRule(List<Part> syntax, String definition) {
        super(syntax, definition);
    }

    @Override
    public Boolean matches(String relativePath) {
        return true;
    }

    @Override
    protected Operation getExcludeOperation(){ return Operation.EXCLUDE_AND_TERMINATE; }
}
