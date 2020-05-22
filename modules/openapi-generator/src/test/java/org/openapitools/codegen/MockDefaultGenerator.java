/*
 * Copyright 2018 OpenAPI-Generator Contributors (https://openapi-generator.tech)
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
import org.openapitools.codegen.templating.TemplateManagerOptions;

import java.io.File;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * Decorates {@link DefaultCodegen and tracks some internal calls}.
 *
 * @deprecated Please avoid using this type, as it is not a mock and invokes real generation. Prefer {@link DefaultGenerator#DefaultGenerator(Boolean)} with dryRun=true and/or true mocked spies.
 */
@Deprecated
public class MockDefaultGenerator extends DefaultGenerator {
    public List<WrittenTemplateBasedFile> getTemplateBasedFiles() {
        return templateBasedFiles;
    }

    public Map<String, String> getFiles() {
        return files;
    }

    //        public static final String INPUT_STREAM_CONTENT = "INPUT STREAM CONTENT";
    private List<WrittenTemplateBasedFile> templateBasedFiles = new ArrayList<>();
    private Map<String, String> files = new HashMap<>();

    public MockDefaultGenerator() {
        super(true);
    }

    public MockDefaultGenerator(boolean dryRun) {
        super(dryRun);
    }

    @Override
    public Generator opts(ClientOptInput opts) {
        Generator o = super.opts(opts);
        TemplateManagerOptions templateManagerOptions = new TemplateManagerOptions(this.config.isEnableMinimalUpdate(),this.config.isSkipOverwrite());
        this.templateProcessor = new ObservableDryRunTemplateManager(templateManagerOptions);
        return o;
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

    class ObservableDryRunTemplateManager extends DryRunTemplateManager {
        public ObservableDryRunTemplateManager(TemplateManagerOptions options) {
            super(options);
        }

        private String normalizePath(String filename) {
            return filename.replace("\\", "/").replace("//", "/");
        }

        @Override
        public File write(Map<String, Object> data, String template, File target) throws IOException {
            String filename = normalizePath(target.toPath().normalize().toString());
            templateBasedFiles.add(new WrittenTemplateBasedFile(data, template, filename));

            File file = super.write(data, template, target);
            if (file != null && file.exists()) {
                byte[] contents = Files.readAllBytes(file.toPath());
                files.put(normalizePath(filename), new String(contents, StandardCharsets.UTF_8));
            }

            return file;
        }

        @Override
        public File writeToFile(String filename, byte[] contents) throws IOException {
            files.put(normalizePath(filename), new String(contents, StandardCharsets.UTF_8));
            return super.writeToFile(filename, contents);
        }
    }
}
