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

package org.openapitools.codegen;

import java.util.Objects;

public class SupportingFile {
    public String templateFile;
    public String folder;
    public String destinationFilename;
    public boolean canOverwrite = true;

    public SupportingFile(String templateFile, String destinationFilename) {
        this(templateFile, "", destinationFilename);
    }

    public SupportingFile(String templateFile, String folder, String destinationFilename) {
        this.templateFile = templateFile;
        this.folder = folder;
        this.destinationFilename = destinationFilename;
    }

    @Override
    public int hashCode() {
        return Objects.hash(templateFile, folder, destinationFilename, canOverwrite);
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;

        SupportingFile that = (SupportingFile) o;

        return Objects.equals(templateFile, that.templateFile) &&
                Objects.equals(folder, that.folder) &&
                Objects.equals(destinationFilename, that.destinationFilename) &&
                canOverwrite == that.canOverwrite;
    }

    @SuppressWarnings("StringBufferReplaceableByString")
    @Override
    public String toString() {
        StringBuilder builder = new StringBuilder();
        builder.append("SupportingFile:").append("\n");
        builder.append("\ttemplateFile: ").append(templateFile).append("\n");
        builder.append("\tfolder: ").append(folder).append("\n");
        builder.append("\tcanOverwrite: ").append(Boolean.valueOf(canOverwrite)).append("\n");
        builder.append("\tdestinationFilename: ").append(destinationFilename).append("\n");

        return builder.toString();
    }

    /**
     * Identifies this instance as referring to a supporting file which should not overwrite a file of the same name.
     *
     * @return This object, for chaining.
     */
    public SupportingFile doNotOverwrite() {
        canOverwrite = false;
        return this;
    }
}


