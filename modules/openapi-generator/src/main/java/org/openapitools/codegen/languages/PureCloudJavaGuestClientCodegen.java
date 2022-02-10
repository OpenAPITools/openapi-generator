package org.openapitools.codegen.languages;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;


public class PureCloudJavaGuestClientCodegen extends PureCloudJavaClientCodegen {
    protected Logger LOGGER = LoggerFactory.getLogger(PureCloudJavaGuestClientCodegen.class);

    public PureCloudJavaGuestClientCodegen() {
        super();

        // Custom mappings for swagger type -> java type
        // Override the standard sdk's type
        importMapping.replace("PagedResource", "com.mypurecloud.sdk.v2.guest.PagedResource");
        importMapping.put("LocalDate", "java.time.LocalDate");

        // Type overrides
        typeMapping.put("date", "LocalDate");
    }

    @Override
    public String getName() { return "purecloudjava-guest"; }
}
