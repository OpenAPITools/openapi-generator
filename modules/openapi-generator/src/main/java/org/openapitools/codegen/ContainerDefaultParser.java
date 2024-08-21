package org.openapitools.codegen;

import lombok.Data;

import java.util.ArrayList;
import java.util.List;

class ContainerDefaultParser {

    protected static final String REQUIRED = "required";
    protected static final String NULLABLE = "nullable";
    protected static final String ERROR_UNEXPECTED = "unexpected containerDefaultToNull keyword: ";

    protected static List<Condition> parseExpression(String input) {
        if (input == null || "".equals(input.trim())) {
            input = "7.5.0";
        }

        String[] orConditions = input.split("\\|");
        List<Condition> conditions = new ArrayList<>();

        for (String myOrCondition : orConditions) {
            conditions.addAll(parseCondition(myOrCondition.trim()));
        }

        return conditions;
    }

    protected static List<Condition> parseCondition(String conditionStr) {
        String[] andConditions = conditionStr.split("&");
        Boolean required = null;
        NullableState nullable = null;

        for (String myAndCondition : andConditions) {
            String trimmed = myAndCondition.trim();

            if (trimmed.startsWith("!")) {
                String keyword = trimmed.substring(1).trim();
                if (keyword.equals(REQUIRED)) {
                    required = false;
                } else if (keyword.equals(NULLABLE)) {
                    nullable = NullableState.NO;
                } else {
                    throw new IllegalArgumentException(ERROR_UNEXPECTED + keyword);
                }
            } else if (trimmed.startsWith("?")) {
                String keyword = trimmed.substring(1).trim();
                if (keyword.equals(NULLABLE)) {
                    nullable = NullableState.UNSPECIFIED;
                } else {
                    throw new IllegalArgumentException(ERROR_UNEXPECTED + keyword);
                }
            } else {
                switch (trimmed) {
                    case REQUIRED:
                        required = true;
                        break;
                    case NULLABLE:
                        nullable = NullableState.YES;
                        break;
                    case "7.5.0":
                    case "false":
                        return List.of(
                                new Condition(true, NullableState.YES),
                                new Condition(false, NullableState.YES));
                    case "7.4.0":
                        return List.of(
                                new Condition(true, NullableState.YES),
                                new Condition(false, NullableState.YES),
                                new Condition(false, NullableState.NO),
                                new Condition(false, NullableState.UNSPECIFIED));
                    case "true":
                        return List.of(
                                new Condition(true, NullableState.YES),
                                new Condition(true, NullableState.NO),
                                new Condition(true, NullableState.UNSPECIFIED),
                                new Condition(false, NullableState.YES),
                                new Condition(false, NullableState.NO),
                                new Condition(false, NullableState.UNSPECIFIED));
                    case "none":
                        return new ArrayList<>();
                    default:
                        throw new IllegalArgumentException(ERROR_UNEXPECTED + trimmed);
                }
            }
        }

        return List.of(new Condition(required, nullable));
    }

    protected enum NullableState {
        YES, NO, UNSPECIFIED
    }

    @Data
    protected static class Condition {
        private final Boolean required;
        private final NullableState nullable;
    }

}
