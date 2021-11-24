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

import org.openapitools.codegen.api.TemplateDefinition;
import org.openapitools.codegen.api.TemplateFileType;

import java.util.Objects;
import java.util.StringJoiner;

/**
 * Defines the template definition for a "supporting file", that is any file which is generic and not bound to
 * api/model definitions and their relevant docs or tests.
 *
 * Supporting files are generated once for an entire application while api/model bound definitions are generated multiple
 * times according to their target use.
 */
public class SupportingFile extends TemplateDefinition {
    private boolean canOverwrite = true;

    public SupportingFile(String templateFile, String destinationFilename) {
        this(templateFile, "", destinationFilename);
    }

    public SupportingFile(String templateFile, String folder, String destinationFilename) {
        super(templateFile, folder, destinationFilename);
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

    /**
     * Sets the type of template
     *
     * @param templateType a {@link TemplateFileType} enum which defines the type of this template
     */
    @Override
    public void setTemplateType(TemplateFileType templateType) {

    }

    /**
     * Gets the type of template
     *
     * @return a {@link TemplateFileType} enum which defines the type of this template.
     */
    @Override
    public TemplateFileType getTemplateType() {
        return TemplateFileType.SupportingFiles;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (!(o instanceof SupportingFile)) return false;
        if (!super.equals(o)) return false;
        SupportingFile that = (SupportingFile) o;
        return isCanOverwrite() == that.isCanOverwrite();
    }

    public boolean isCanOverwrite() {
        return canOverwrite;
    }

    @Override
    public int hashCode() {
        return Objects.hash(super.hashCode(), isCanOverwrite());
    }

    @Override
    public String toString() {
        return new StringJoiner(", ", SupportingFile.class.getSimpleName() + "[", "]")
                .add("templateFile='" + getTemplateFile() + "'")
                .add("folder='" + getFolder() + "'")
                .add("destinationFilename='" + getDestinationFilename() + "'")
                .add("canOverwrite=" + isCanOverwrite())
                .toString();
    }
}


