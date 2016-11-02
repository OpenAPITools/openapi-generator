package io.swagger.codegen.languages;

import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.testng.Assert;
import org.testng.annotations.Test;

public class JavaClientCodegenTest {
	
	private static final String VENDOR_MIME_TYPE = "application/vnd.company.v1+json";
	private static final String XML_MIME_TYPE = "application/xml";
	private static final String JSON_MIME_TYPE = "application/json";
	private static final String TEXT_MIME_TYPE = "text/plain";

	@Test
	public void testJsonMime() {
		Assert.assertTrue(JavaClientCodegen.isJsonMimeType(JSON_MIME_TYPE));
                Assert.assertFalse(JavaClientCodegen.isJsonMimeType(XML_MIME_TYPE));
                Assert.assertFalse(JavaClientCodegen.isJsonMimeType(TEXT_MIME_TYPE));
                
		Assert.assertTrue(JavaClientCodegen.isJsonVendorMimeType("application/vnd.mycompany+json"));
		Assert.assertTrue(JavaClientCodegen.isJsonVendorMimeType("application/vnd.mycompany.v1+json"));
		Assert.assertTrue(JavaClientCodegen.isJsonVendorMimeType("application/vnd.mycompany.resourceTypeA.version1+json"));
		Assert.assertTrue(JavaClientCodegen.isJsonVendorMimeType("application/vnd.mycompany.resourceTypeB.version2+json"));	
		Assert.assertFalse(JavaClientCodegen.isJsonVendorMimeType("application/v.json"));
		
	}
	
	@Test
	public void testContentTypePrioritization() {
            Map<String, String> jsonMimeType = new HashMap<>();
            jsonMimeType.put(JavaClientCodegen.MEDIA_TYPE, JSON_MIME_TYPE);

            Map<String, String> xmlMimeType = new HashMap<>();
            xmlMimeType.put(JavaClientCodegen.MEDIA_TYPE, XML_MIME_TYPE);

            Map<String, String> vendorMimeType = new HashMap<>();
            vendorMimeType.put(JavaClientCodegen.MEDIA_TYPE, VENDOR_MIME_TYPE);

            Map<String, String> textMimeType = new HashMap<>();
            textMimeType.put(JavaClientCodegen.MEDIA_TYPE, TEXT_MIME_TYPE);

            Assert.assertEquals(JavaClientCodegen.prioritizeContentTypes(
                    Collections.<Map<String,String>>emptyList()), Collections.emptyList());

            Assert.assertEquals(JavaClientCodegen.prioritizeContentTypes(Arrays.asList(xmlMimeType)), Arrays.asList(xmlMimeType));
            Assert.assertEquals(JavaClientCodegen.prioritizeContentTypes(Arrays.asList(jsonMimeType)), Arrays.asList(jsonMimeType));
            Assert.assertEquals(JavaClientCodegen.prioritizeContentTypes(Arrays.asList(vendorMimeType)), Arrays.asList(vendorMimeType));

            
            Assert.assertEquals(JavaClientCodegen.prioritizeContentTypes(Arrays.asList(xmlMimeType, jsonMimeType)), 
                    Arrays.asList(jsonMimeType, xmlMimeType));
            Assert.assertEquals(JavaClientCodegen.prioritizeContentTypes(Arrays.asList(jsonMimeType, xmlMimeType)), 
                    Arrays.asList(jsonMimeType, xmlMimeType));
            Assert.assertEquals(JavaClientCodegen.prioritizeContentTypes(Arrays.asList(jsonMimeType, vendorMimeType)), 
                    Arrays.asList(vendorMimeType, jsonMimeType));
            Assert.assertEquals(JavaClientCodegen.prioritizeContentTypes(Arrays.asList(textMimeType, xmlMimeType)), 
                    Arrays.asList(textMimeType, xmlMimeType));
            Assert.assertEquals(JavaClientCodegen.prioritizeContentTypes(Arrays.asList(xmlMimeType, textMimeType)), 
                    Arrays.asList(xmlMimeType, textMimeType));
            
	    System.out.println(JavaClientCodegen.prioritizeContentTypes(Arrays.asList(
	            xmlMimeType,textMimeType, jsonMimeType, vendorMimeType)));
	    
	    List<Map<String,String>> priContentTypes = JavaClientCodegen.prioritizeContentTypes(Arrays.asList(
                    xmlMimeType, textMimeType, jsonMimeType, vendorMimeType));
	    Assert.assertEquals(priContentTypes, Arrays.asList(vendorMimeType, jsonMimeType, xmlMimeType, textMimeType));
	    
	    for ( int i = 0; i < 3; i++ )
                Assert.assertNotNull(priContentTypes.get(i).get("hasMore"));
            Assert.assertNull(priContentTypes.get(3).get("hasMore"));
	    	    
	}
	
}
