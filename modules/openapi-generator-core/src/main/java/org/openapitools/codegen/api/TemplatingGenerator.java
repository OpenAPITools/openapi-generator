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

package org.openapitools.codegen.api;

// TODO: 6.0 Remove
/**
 * interface to the full template content
 * implementers might take into account the -t cli option,
 * look in the resources for a language specific template, etc
 *
 * @deprecated as of 5.0, replaced by {@link TemplatingExecutor}.
 */
@Deprecated
public interface TemplatingGenerator extends TemplatingExecutor {

}
