package org.openapitools.codegen.validations.oas;

import io.swagger.v3.oas.models.PathItem;
import io.swagger.v3.oas.models.parameters.RequestBody;
import org.apache.commons.lang3.StringUtils;
import org.openapitools.codegen.validation.GenericValidator;
import org.openapitools.codegen.validation.ValidationRule;

import java.util.ArrayList;
import java.util.Locale;

/**
 * A standalone instance for evaluating rule and recommendations related to OAS {@link io.swagger.v3.oas.models.Operation}
 */
class OpenApiOperationValidations extends GenericValidator<OperationWrapper> {
    OpenApiOperationValidations(RuleConfiguration ruleConfiguration) {
        super(new ArrayList<>());
        if (ruleConfiguration.isEnableRecommendations()) {
            if (ruleConfiguration.isEnableApiRequestUriWithBodyRecommendation()) {
                rules.add(ValidationRule.warn(
                        "API GET/HEAD defined with request body",
                        "While technically allowed, GET/HEAD with request body may indicate programming error, and is considered an anti-pattern.",
                        OpenApiOperationValidations::checkAntipatternGetOrHeadWithBody
                ));
            }
        }
    }

    /**
     * Determines whether a GET or HEAD operation is configured to expect a body.
     * <p>
     * RFC7231 describes this behavior as:
     * <p>
     * A payload within a GET request message has no defined semantics;
     * sending a payload body on a GET request might cause some existing
     * implementations to reject the request.
     * <p>
     * See https://tools.ietf.org/html/rfc7231#section-4.3.1
     * <p>
     * Because there are no defined semantics, and because some client and server implementations
     * may silently ignore the entire body (see https://xhr.spec.whatwg.org/#the-send()-method) or
     * throw an error (see https://fetch.spec.whatwg.org/#ref-for-dfn-throw%E2%91%A1%E2%91%A1),
     * we maintain that the existence of a body for this operation is most likely programmer error and raise awareness.
     *
     * @param wrapper Wraps an operation with accompanying HTTP Method
     * @return {@link ValidationRule.Pass} if the check succeeds, otherwise {@link ValidationRule.Fail}
     */
    private static ValidationRule.Result checkAntipatternGetOrHeadWithBody(OperationWrapper wrapper) {
        if (wrapper == null) {
            return ValidationRule.Pass.empty();
        }

        ValidationRule.Result result = ValidationRule.Pass.empty();

        if (wrapper.getHttpMethod() == PathItem.HttpMethod.GET || wrapper.getHttpMethod() == PathItem.HttpMethod.HEAD) {
            RequestBody body = wrapper.getOperation().getRequestBody();

            if (body != null) {
                if (StringUtils.isNotEmpty(body.get$ref()) || (body.getContent() != null && body.getContent().size() > 0)) {
                    result = new ValidationRule.Fail();
                    result.setDetails(String.format(
                            Locale.ROOT,
                            "%s %s contains a request body and is considered an anti-pattern.",
                            wrapper.getHttpMethod().name(),
                            wrapper.getOperation().getOperationId())
                    );
                }
            }

        }

        return result;
    }

}
