package org.openapitools.codegen.options;

import com.google.common.collect.ImmutableMap;
import org.openapitools.codegen.CodegenConstants;

import java.util.Map;

public class GoAfterShipClientOptionsProvider implements OptionsProvider {
        @Override
        public String getLanguage() {
            return "go-aftership";
        }

        @Override
        public Map<String, String> createOptions() {
            ImmutableMap.Builder<String, String> builder = new ImmutableMap.Builder<String, String>();
            return builder
                    .put(CodegenConstants.IS_GO_SUBMODULE, "true")
                    .put(CodegenConstants.HIDE_GENERATION_TIMESTAMP, "true")
                    .put(CodegenConstants.ENUM_CLASS_PREFIX, "true")
                    .put(CodegenConstants.PREPEND_FORM_OR_BODY_PARAMETERS, "true")
                    .put(CodegenConstants.WITH_AWSV4_SIGNATURE_COMMENT, "true")
                    .put(CodegenConstants.DISALLOW_ADDITIONAL_PROPERTIES_IF_NOT_PRESENT, "true")
                    .put(CodegenConstants.USE_ONEOF_DISCRIMINATOR_LOOKUP, "true")
                    .put("generateInterfaces", "true")
                    .put("structPrefix", "true")
                    .build();
        }

        @Override
        public boolean isServer() {
            return false;
        }
}
