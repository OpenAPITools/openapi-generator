/*
 * Copyright 2018 OpenAPI-Generator Contributors (https://openapi-generator.tech)
 * Copyright 2018 SmartBear Software
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

package org.openapitools.codegen.ignore.rules;

import java.nio.file.FileSystems;
import java.nio.file.PathMatcher;
import java.util.List;

public class DirectoryRule extends FileRule {

    private PathMatcher directoryMatcher = null;
    private PathMatcher contentsMatcher = null;

    DirectoryRule(List<Part> syntax, String definition) {
        super(syntax, definition);
        String pattern = this.getPattern();
        StringBuilder sb = new StringBuilder();
        sb.append("glob:");
        sb.append(pattern);
        if(!pattern.endsWith("/")) sb.append("/");
        directoryMatcher = FileSystems.getDefault().getPathMatcher(sb.toString());
        sb.append("**");
        contentsMatcher = FileSystems.getDefault().getPathMatcher(sb.toString());
    }

    @Override
    public Boolean matches(String relativePath) {
        return contentsMatcher.matches(FileSystems.getDefault().getPath(relativePath)) || directoryMatcher.matches(FileSystems.getDefault().getPath(relativePath));
    }
}
