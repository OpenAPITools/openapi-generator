package org.openapitools.codegen.languages.features;

import java.io.File;

public interface CXFExtServerFeatures extends CXFServerFeatures {

    String GENERATE_OPERATION_BODY = "generateOperationBody";
    String SUPPORT_MULTIPLE_SPRING_SERVICES = "supportMultipleSpringServices";
    String TEST_DATA_FILE = "testDataFile";
    String TEST_DATA_CONTROL_FILE = "testDataControlFile";

    void setGenerateOperationBody(boolean generateOperationBody);

    void setLoadTestDataFromFile(boolean loadTestDataFromFile);

    void setTestDataFile(File testDataFile);

    void setTestDataControlFile(File testDataControlFile);

}