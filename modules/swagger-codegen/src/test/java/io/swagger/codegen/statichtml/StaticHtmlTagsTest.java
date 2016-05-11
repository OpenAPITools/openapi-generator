package io.swagger.codegen.statichtml;

import static org.testng.Assert.assertEquals;

import java.io.File;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import javax.annotation.Nullable;

import org.apache.commons.lang3.StringUtils;
import org.junit.rules.TemporaryFolder;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.Test;

import com.google.common.base.Function;
import com.google.common.collect.Lists;

import io.swagger.codegen.ClientOptInput;
import io.swagger.codegen.ClientOpts;
import io.swagger.codegen.CodegenConfig;
import io.swagger.codegen.DefaultGenerator;
import io.swagger.codegen.languages.StaticHtmlGenerator;
import io.swagger.models.Operation;
import io.swagger.models.Path;
import io.swagger.models.Swagger;
import io.swagger.parser.SwaggerParser;

public class StaticHtmlTagsTest {

    public TemporaryFolder folder = new TemporaryFolder();

    @BeforeMethod
    public void setUp() throws Exception {
        folder.create();
    }

    @AfterMethod
    public void tearDown() throws Exception {
        folder.delete();
    }
    
    @Test 
    public void testApiTags() throws Exception {
        final Swagger swagger = new SwaggerParser().read("src/test/resources/2_0/petstore.json");

        final int maxTagsToTest = 2; // how to flip it randomly from 2 to 1, and shuffle ops?
        // if an op has a few tags it will be duplicated here, but it's exactly what we expect in doc
        final List<Operation> expectedOperations = new ArrayList<Operation>();
        
        final String capitalCommatizedTags = pickupFewTagsAndOps(swagger,
                maxTagsToTest, expectedOperations);
        
        final Collection<Object> seenOperations = new ArrayList<Object>();
        CodegenConfig codegenConfig =  new StaticHtmlGenerator(){ // new StaticDocCodegen(){
            public  Map<String, Object> postProcessSupportingFileData(Map<String, Object> objs)  {
                //System.out.println(getOperations(objs));
                final Collection<?> actualOperations = getOperations(objs);
                seenOperations.addAll(actualOperations);
                assertEquals(actualOperations.size(), expectedOperations.size(), 
                        "Expectig the same size of ops for -Dapis="+capitalCommatizedTags +
                        " in fact, actual "+actualOperations+" doesn't seem like expecting " 
                                + expectedOperations);
                return objs;
            }
        };
        codegenConfig.setOutputDir(folder.getRoot().getAbsolutePath());
            
        ClientOptInput clientOptInput = new ClientOptInput().opts(new ClientOpts()).swagger(swagger)
                .config(codegenConfig);

        final String apisBackup = System.setProperty("apis", capitalCommatizedTags);
        try {
            DefaultGenerator gen = new DefaultGenerator();
            gen.opts(clientOptInput);
            gen.generate();
            assertEquals(seenOperations.isEmpty(), false, 
                    "something has been changed in code and now code bypass the mock above...");
        } finally {
            if (apisBackup!=null) {
                System.setProperty("apis", apisBackup);
            }else{
                System.clearProperty("apis");  
            }
        }
    }

    protected String pickupFewTagsAndOps(final Swagger swagger,
            final int maxTagsToTest, final Collection<Operation> expectedOperations) {
        Set<String> expectedTags = new HashSet<String>();
        for ( Path path:swagger.getPaths().values() ) {
            for ( Operation op : path.getOperations() ) {
                for ( String tag : op.getTags() ) {
                    if (expectedTags.size() < maxTagsToTest) {
                        expectedTags.add(tag);
                        expectedOperations.add(op);
                    } else {
                        if ( expectedTags.contains(tag) ) {
                            expectedOperations.add(op);
                        }
                    }
                }
            }
        }
        
        final String capitalCommatizedTags = StringUtils.join(
                Lists.transform(Lists.newArrayList(expectedTags), 
                        new Function<String, String>() { 
            @Nullable
            @Override
            public String apply(final String input) {
                return StringUtils.capitalize(input);
            }
        }), ",");
        return capitalCommatizedTags;
    }
    
    @SuppressWarnings({ "rawtypes", "unchecked" })
    protected static Collection getOperations(Map<String, Object> objs) {
        final ArrayList rez = new ArrayList();
        final Map apiInfo = (Map)objs.get("apiInfo");
        for(Object apiElem : ((List)apiInfo.get("apis"))){ 
            Map<String, Object> api = (Map<String, Object>) apiElem;
            rez.addAll( (Collection) // what if the same op goes on two tags??
            ((Map)api.get("operations")).get("operation"));
        }
        return rez;
    }
}
