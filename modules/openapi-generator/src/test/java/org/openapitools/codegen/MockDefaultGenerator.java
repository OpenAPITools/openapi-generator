/*
 * Copyright 2018 OpenAPI-Generator Contributors (https://openapi-generator.tech)
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.openapitools.codegen;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class MockDefaultGenerator extends DefaultGenerator {
    public static final String INPUT_STREAM_CONTENT = "INPUT STREAM CONTENT";
    private List<WrittenTemplateBasedFile> templateBasedFiles = new ArrayList<>();
    private Map<String, String> files = new HashMap<>();

    @Override
    protected File processTemplateToFile(Map<String, Object> templateData, String templateName, String outputFilename) throws IOException {
        templateBasedFiles.add(new WrittenTemplateBasedFile(templateData, templateName, normalizePath(outputFilename)));
        return super.processTemplateToFile(templateData, templateName, outputFilename);
    }

    @Override
    protected File writeInputStreamToFile(String filename, InputStream in, String templateFile) throws FileNotFoundException, IOException {
        files.put(normalizePath(filename), INPUT_STREAM_CONTENT + ": from template '" + templateFile + "'");
        return new File(filename);
    }

    @Override
    public File writeToFile(String filename, String contents) throws IOException {
        files.put(normalizePath(filename), contents);
        return new File(filename);
    }

    private String normalizePath(String filename) {
        return filename.replace("\\", "/").replace("//", "/");
    }

    public List<WrittenTemplateBasedFile> getTemplateBasedFiles() {
        return templateBasedFiles;
    }

    public Map<String, String> getFiles() {
        return files;
    }

    public static class WrittenTemplateBasedFile {
        private Map<String, Object> templateData;
        private String templateName;
        private String outputFilename;

        public WrittenTemplateBasedFile(Map<String, Object> templateData, String templateName, String outputFilename) {
            this.templateData = templateData;
            this.templateName = templateName;
            this.outputFilename = outputFilename;
        }

        public Map<String, Object> getTemplateData() {
            return templateData;
        }

        public String getTemplateName() {
            return templateName;
        }

        public String getOutputFilename() {
            return outputFilename;
        }

        @Override
        public String toString() {
            return "WrittenTemplateBasedFile [" +
                    "outputFilename=" + outputFilename + ", " +
                    "templateName=" + templateName +  ", " +
                    "templateData=" + templateData + "]";
        }
    }
}
