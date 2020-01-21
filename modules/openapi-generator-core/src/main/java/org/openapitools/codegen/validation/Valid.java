/*
 * Copyright 2019 OpenAPI-Generator Contributors (https://openapi-generator.tech)
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

package org.openapitools.codegen.validation;

/**
 * Represents a {@link Validated} state which is "valid" according to the defined rule.
 */
public final class Valid extends Validated {
    private ValidationRule rule;

    /**
     * Defines whether or not the validation resulted in a "valid" condition.
     *
     * @return <code>true</code> if the instance passed validation of the rule returned by {@link Validated#getRule()}.
     */
    @Override
    boolean isValid() {
        return true;
    }

    /**
     * Constructs a new {@link Valid} instance.
     *
     * @param rule The rule which was evaluated and resulted in this state.
     */
    Valid(ValidationRule rule) {
        this.rule = rule;
    }

    @Override
    public String getMessage() {
        return null;
    }

    @Override
    public ValidationRule getRule() {
        return rule;
    }
}
