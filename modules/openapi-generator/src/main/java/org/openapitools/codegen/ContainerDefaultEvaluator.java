package org.openapitools.codegen;

import io.swagger.v3.oas.models.media.Schema;

import java.util.List;

public class ContainerDefaultEvaluator {

    final List<ContainerDefaultParser.Condition> conditions;

    public ContainerDefaultEvaluator(String containerDefaultToNull) {
        conditions = ContainerDefaultParser.parseExpression(containerDefaultToNull);
    }

    public boolean isNullDefault(CodegenProperty cp, Schema schema) {
        ContainerDefaultParser.NullableState nullable;
        if (schema.getNullable() == null && (schema.getExtensions() == null || !schema.getExtensions().containsKey("x-nullable"))) {
            nullable = ContainerDefaultParser.NullableState.UNSPECIFIED;
        } else {
            if (cp.isNullable) {
                nullable = ContainerDefaultParser.NullableState.YES;
            } else {
                nullable = ContainerDefaultParser.NullableState.NO;
            }
        }

        return conditions.stream().anyMatch(
                myCondition ->
                        (myCondition.getRequired() == null || myCondition.getRequired().equals(cp.required))
                                && (myCondition.getNullable() == null || myCondition.getNullable().equals(nullable))
        );
    }

}
