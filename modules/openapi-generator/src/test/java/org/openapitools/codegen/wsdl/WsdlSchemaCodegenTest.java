package org.openapitools.codegen.wsdl;

import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.oas.models.Operation;
import org.apache.commons.io.FileUtils;
import org.openapitools.codegen.ClientOptInput;
import org.openapitools.codegen.CodegenOperation;
import org.openapitools.codegen.DefaultGenerator;
import org.openapitools.codegen.TestUtils;
import org.openapitools.codegen.languages.WsdlSchemaCodegen;
import org.testng.Assert;
import org.testng.annotations.AfterClass;
import org.testng.annotations.BeforeClass;
import org.testng.annotations.Test;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.List;

import static org.openapitools.codegen.TestUtils.assertFileContains;
import static org.openapitools.codegen.TestUtils.ensureContainsFile;

public class WsdlSchemaCodegenTest {
    private OpenAPI openAPI;
    private File outputDirectory;
    private String outputPath;
    private List<File> listOfFiles;

    @BeforeClass
    public void setUp() throws IOException {
        this.openAPI = TestUtils.parseFlattenSpec("src/test/resources/3_0/wsdl/petstore.yaml");
        this.outputDirectory = Files.createTempDirectory("test").toFile().getCanonicalFile();
        this.outputPath = this.outputDirectory.getAbsolutePath().replace('\\', '/');

        WsdlSchemaCodegen codegen = new WsdlSchemaCodegen();
        codegen.setOutputDir(this.outputDirectory.getAbsolutePath());

        ClientOptInput input = new ClientOptInput()
                .openAPI(this.openAPI)
                .config(codegen);

        DefaultGenerator generator = new DefaultGenerator();
        this.listOfFiles = generator.opts(input).generate();
    }

    @Test(description = "ensure that the operationid has been generated correctly")
    public void testOperationIdGeneration() {
        final OpenAPI openAPI = this.openAPI;
        WsdlSchemaCodegen codegen = new WsdlSchemaCodegen();
        codegen.setOpenAPI(openAPI);

        String requestPathWithId = "/store/order/{orderId}";
        Operation textOperationGet = openAPI.getPaths().get(requestPathWithId).getGet();
        CodegenOperation opGet = codegen.fromOperation(requestPathWithId, "get", textOperationGet, null);
        String newOperationIdWithId = codegen.generateOperationId(opGet);

        String requestPathWithoutId = "/store/order";
        Operation textOperationPost = openAPI.getPaths().get(requestPathWithoutId).getPost();
        CodegenOperation opPost = codegen.fromOperation(requestPathWithoutId, "post", textOperationPost, null);
        String newOperationIdWithoutId = codegen.generateOperationId(opPost);

        Assert.assertEquals(newOperationIdWithId, "GetStoreOrderByOrderid");
        Assert.assertEquals(newOperationIdWithoutId, "PostStoreOrder");
    }

    @Test(description = "Ensure that passed strings are processed correctly by this method")
    public void testLowerCaseStringExceptFirstLetter() {
        WsdlSchemaCodegen codegen = new WsdlSchemaCodegen();
        String value = codegen.lowerCaseStringExceptFirstLetter("uploadPetByPathId");

        Assert.assertEquals(value, "Uploadpetbypathid");
    }

    @Test(description = "Check if element tags has been created for an operation ")
    public void testIfElementTagsExist() {
        String xsElementRequestMessage =
                "<xs:element name=\"PostPetByPetid_RequestMessage\" type=\"schemas:PostPetByPetid_RequestMessage\" />";
        String xsElementResponseMessage =
                "<xs:element name=\"PostPetByPetid_ResponseMessage\" type=\"schemas:PostPetByPetid_ResponseMessage\" />";
        String xsElementErrorResponse =
                " <xs:element name=\"PostPetByPetid_405\">\n"
                        + "   <xs:annotation>\n"
                        + "     <xs:documentation>Invalid input</xs:documentation>\n"
                        + "   </xs:annotation>\n"
                        + " </xs:element>\n";

        assertFileContains(Paths.get(this.outputPath + "/service.wsdl"), xsElementRequestMessage);
        assertFileContains(Paths.get(this.outputPath + "/service.wsdl"), xsElementResponseMessage);
        assertFileContains(Paths.get(this.outputPath + "/service.wsdl"), xsElementErrorResponse);
    }

    @Test(description = "Check if complexType input- and output-message has been created for an operation ")
    public void testIfInputAndResponseMessageExist() {
        String complexTypeRequestMessage =
                " <xs:complexType name=\"GetPetByPetid_RequestMessage\">\n"
                        + "   <xs:sequence>\n"
                        + "     <xs:element minOccurs=\"1\" name=\"petId\" type=\"xs:long\">\n"
                        + "       <xs:annotation>\n"
                        + "         <xs:documentation>ID of pet to return</xs:documentation>\n"
                        + "       </xs:annotation>\n"
                        + "     </xs:element>\n"
                        + "   </xs:sequence>\n"
                        + " </xs:complexType>\n";

        String complexTypeResponseMessage =
                " <xs:complexType name=\"GetPetByPetid_ResponseMessage\">\n"
                        + "   <xs:sequence>\n"
                        + "     <xs:element minOccurs=\"1\" name=\"Pet\" type=\"schemas:Pet\">\n"
                        + "       <xs:annotation>\n"
                        + "         <xs:documentation>successful operation</xs:documentation>\n"
                        + "       </xs:annotation>\n"
                        + "     </xs:element>\n"
                        + "   </xs:sequence>\n"
                        + " </xs:complexType>\n";

        assertFileContains(Paths.get(this.outputPath + "/service.wsdl"), complexTypeRequestMessage);
        assertFileContains(Paths.get(this.outputPath + "/service.wsdl"), complexTypeResponseMessage);
    }

    @Test(description =
            "Check if complexType RequestMessage with minimum and maximum restriction has been created for an operation ")
    public void testIfRequestMessageMinimumExists() {
        String complexTypeRequestMessageMinimum =
                " <xs:complexType name=\"GetStoreOrderByOrderid_RequestMessage\">\n"
                        + "   <xs:sequence>\n"
                        + "     <xs:element minOccurs=\"1\" name=\"orderId\">\n"
                        + "       <xs:simpleType>\n"
                        + "         <xs:annotation>\n"
                        + "           <xs:documentation>ID of pet that needs to be fetched</xs:documentation>\n"
                        + "         </xs:annotation>\n"
                        + "         <xs:restriction base=\"xs:long\">\n"
                        + "           <xs:maxInclusive value=\"10\" />\n"
                        + "           <xs:minInclusive value=\"1\" />\n"
                        + "         </xs:restriction>\n"
                        + "       </xs:simpleType>\n"
                        + "     </xs:element>\n"
                        + "   </xs:sequence>\n"
                        + " </xs:complexType>\n";

        assertFileContains(Paths.get(this.outputPath + "/service.wsdl"), complexTypeRequestMessageMinimum);
    }

    @Test(description = "Check if complexType model has been created for an openapi model schema")
    public void testIfComplexTypeModelExists() {
        String complexTypeModel =
                " <xs:complexType name=\"Pet\">\n"
                        + "   <xs:sequence>\n"
                        + "     <xs:element minOccurs=\"0\" name=\"id\" type=\"xs:long\" />\n"
                        + "     <xs:element minOccurs=\"0\" name=\"category\" type=\"schemas:Category\" />\n"
                        + "     <xs:element minOccurs=\"1\" name=\"name\" type=\"xs:string\" />\n"
                        + "     <xs:element minOccurs=\"1\" maxOccurs=\"unbounded\" name=\"photoUrls\" type=\"xs:string\" />\n"
                        + "     <xs:element minOccurs=\"0\" maxOccurs=\"unbounded\" name=\"tags\" type=\"schemas:Tag\" />\n"
                        + "     <xs:element minOccurs=\"0\" name=\"status\" type=\"schemas:Status\">\n"
                        + "       <xs:annotation>\n"
                        + "         <xs:documentation>pet status in the store</xs:documentation>\n"
                        + "       </xs:annotation>\n"
                        + "     </xs:element>\n"
                        + "   </xs:sequence>\n"
                        + " </xs:complexType>\n";

        assertFileContains(Paths.get(this.outputPath + "/service.wsdl"), complexTypeModel);
    }

    @Test(description = "Check if message and part tags has been created for an operation ")
    public void testIfMessageTagsAndContentExist() {
        String messageRequestMessage =
                " <message name=\"PostPetByPetid_RequestMessage\">\n"
                        + "   <part name=\"PostPetByPetid_RequestMessage\" element=\"schemas:PostPetByPetid_RequestMessage\" />\n"
                        + " </message>";

        String messageError =
                " <message name=\"PostPetByPetid_405\">\n"
                        + "   <part name=\"PostPetByPetid_405\" element=\"schemas:PostPetByPetid_405\" />\n"
                        + " </message>\n";

        assertFileContains(Paths.get(this.outputPath + "/service.wsdl"), messageRequestMessage);
        assertFileContains(Paths.get(this.outputPath + "/service.wsdl"), messageError);
    }

    @Test(description = "Check if portType tag and portType operation has been generated")
    public void testIfPortTypeOperationExists() {
        String portType = "<portType name=\"ServiceV1_PortType\">";

        String portTypeOperation =
                " <operation name=\"GetPetByPetid\">\n"
                        + "   <documentation>Returns a single pet</documentation>\n"
                        + "   <input message=\"wsdl:GetPetByPetid_RequestMessage\" />\n"
                        + "   <output message=\"wsdl:GetPetByPetid_ResponseMessage\">\n"
                        + "     <documentation>successful operation</documentation>\n"
                        + "   </output>\n"
                        + "   <fault name=\"GetPetByPetid_400\" message=\"wsdl:GetPetByPetid_400\">\n"
                        + "     <documentation>Invalid ID supplied</documentation>\n"
                        + "   </fault>\n"
                        + "   <fault name=\"GetPetByPetid_404\" message=\"wsdl:GetPetByPetid_404\">\n"
                        + "     <documentation>Pet not found</documentation>\n"
                        + "   </fault>\n"
                        + " </operation>\n";

        assertFileContains(Paths.get(this.outputPath + "/service.wsdl"), portType);
        assertFileContains(Paths.get(this.outputPath + "/service.wsdl"), portTypeOperation);
    }

    @Test(description = "Check if portType tag and portType operation has been generated")
    public void testIfBindingOperationExists() {
        String binding = "<binding name=\"ServiceV1_Binding\" type=\"wsdl:ServiceV1_PortType\">";

        String bindingOperation =
                " <operation name=\"GetPetByPetid\">\n"
                        + "   <soap:operation soapAction=\"GetPetByPetid\" xmlns:soap=\"http://schemas.xmlsoap.org/wsdl/soap/\" />\n"
                        + "   <input>\n"
                        + "     <soap:body use=\"literal\" xmlns:soap=\"http://schemas.xmlsoap.org/wsdl/soap/\" />\n"
                        + "   </input>\n"
                        + "   <output>\n"
                        + "     <soap:body use=\"literal\" xmlns:soap=\"http://schemas.xmlsoap.org/wsdl/soap/\" />\n"
                        + "   </output>\n"
                        + "   <fault name=\"GetPetByPetid_400\">\n"
                        + "     <soap:fault use=\"literal\" name=\"GetPetByPetid_400\" xmlns:soap=\"http://schemas.xmlsoap.org/wsdl/soap/\" />\n"
                        + "   </fault>\n"
                        + "   <fault name=\"GetPetByPetid_404\">\n"
                        + "     <soap:fault use=\"literal\" name=\"GetPetByPetid_404\" xmlns:soap=\"http://schemas.xmlsoap.org/wsdl/soap/\" />\n"
                        + "   </fault>\n"
                        + " </operation>\n";

        assertFileContains(Paths.get(this.outputPath + "/service.wsdl"), binding);
        assertFileContains(Paths.get(this.outputPath + "/service.wsdl"), bindingOperation);
    }

    @Test(description = "Ensure that all files have been correctly generated")
    public void testFileGeneration() throws Exception {
        Assert.assertEquals(this.listOfFiles.size(), 5);
        ensureContainsFile(this.listOfFiles, this.outputDirectory, ".openapi-generator-ignore");
        ensureContainsFile(this.listOfFiles, this.outputDirectory, ".openapi-generator/FILES");
        ensureContainsFile(this.listOfFiles, this.outputDirectory, ".openapi-generator/VERSION");
        ensureContainsFile(this.listOfFiles, this.outputDirectory, "service.wsdl");
        ensureContainsFile(this.listOfFiles, this.outputDirectory, "jaxb-customization.xml");
    }

    @Test(description = "Ensure that default description is set if it doesn't exist")
    public void testOpenapiDescriptionWasNotProvided() throws IOException {
        final OpenAPI openAPI = TestUtils.parseFlattenSpec("src/test/resources/3_0/ping.yaml");
        File outputDirectory = Files.createTempDirectory("test").toFile().getCanonicalFile();
        String outputPath = this.outputDirectory.getAbsolutePath().replace('\\', '/');

        WsdlSchemaCodegen codegen = new WsdlSchemaCodegen();
        codegen.setOutputDir(this.outputDirectory.getAbsolutePath());

        ClientOptInput input = new ClientOptInput().openAPI(openAPI).config(codegen);

        DefaultGenerator generator = new DefaultGenerator();
        generator.opts(input).generate();

        String value = "<documentation>No description provided</documentation>";
        assertFileContains(Paths.get(outputPath + "/service.wsdl"), value);

        FileUtils.deleteDirectory(outputDirectory);
    }


    @AfterClass
    public void cleanUp() throws Exception {
        // Delete temp folder
        FileUtils.deleteDirectory(this.outputDirectory);
    }
}
