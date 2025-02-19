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

import java.util.List;
import java.util.regex.Pattern;

/**
 * A special case rule which matches files only if they're located
 * in the same directory as the .openapi-generator-ignore file.
 */
public class RootedFileRule extends Rule {
    private String definedFilename = null;
    private String definedExtension = null;

    RootedFileRule(List<Part> syntax, String definition) {
        super(syntax, definition);

        int separatorIndex = definition.lastIndexOf(".");
        definedFilename = getFilenamePart(definition, separatorIndex);
        definedExtension = getExtensionPart(definition, separatorIndex);
    }

    private String getFilenamePart(final String input, int stopIndex){
        return input.substring('/' == input.charAt(0) ? 1 : 0, stopIndex > 0 ? stopIndex : input.length());
    }

    private String getExtensionPart(final String input, int stopIndex) {
        return input.substring(stopIndex > 0 ? stopIndex+1: input.length());
    }

    @Override
    public Boolean matches(String relativePath) {
        // NOTE: Windows-style separator isn't supported, so File.pathSeparator would be incorrect here.
        // NOTE: lastIndexOf rather than contains because /file.txt is acceptable while path/file.txt is not.
        //       relativePath will be passed by CodegenIgnoreProcessor and is relative to .codegen-ignore.
        boolean isSingleFile = relativePath.lastIndexOf("/") <= 0;

        if(isSingleFile) {
            int separatorIndex = relativePath.lastIndexOf(".");
            final String filename = getFilenamePart(relativePath, separatorIndex);
            final String extension = getExtensionPart(relativePath, separatorIndex);
            boolean extensionMatches = definedExtension.equals(extension) || definedExtension.equals(IgnoreLineParser.Token.MATCH_ANY.getPattern());

            if(extensionMatches && definedFilename.contains(IgnoreLineParser.Token.MATCH_ANY.getPattern())) {
                // TODO: Evaluate any other escape requirements here.
                Pattern regex = Pattern.compile(
                        definedFilename
                                .replaceAll(Pattern.quote("."), "\\\\Q.\\\\E")
                                .replaceAll(Pattern.quote("*"), ".*?") // non-greedy match on 0+ any character
                );
                return regex.matcher(filename).matches();
            }

            return extensionMatches && definedFilename.equals(filename);
        }

        return false;
    }
}
