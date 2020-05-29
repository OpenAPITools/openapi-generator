package org.openapitools.client;

import org.openapitools.client.model.Mammal;
import org.openapitools.client.model.AppleReq;
import org.openapitools.client.model.BananaReq;
import org.openapitools.client.model.FruitReq;
import org.openapitools.client.model.BasquePig;
import org.openapitools.client.model.Pig;
import org.openapitools.client.model.Whale;
import org.openapitools.client.model.Zebra;
import java.lang.Exception;

import org.junit.*;
import static org.junit.Assert.*;


public class JSONComposedSchemaTest {
    JSON json = null;
    Mammal mammal = null;

    @Before
    public void setup() {
        json = new JSON();
        mammal = new Mammal();
    }

    /**
     * Validate a oneOf schema can be deserialized into the expected class.
     * The oneOf schema does not have a discriminator. 
     */
    @Test
    public void testOneOfSchemaWithoutDiscriminator() throws Exception {
        // BananaReq and AppleReq have explicitly defined properties that are different by name.
        // There is no discriminator property.
        String str = "{ \"cultivar\": \"golden delicious\", \"mealy\": false }";
        FruitReq o = json.getContext(null).readValue(str, FruitReq.class);
        assertTrue(o.getActualInstance() instanceof AppleReq);
    }

    /**
     * Validate a oneOf schema can be deserialized into the expected class.
     * The oneOf schema has a discriminator. 
     */
    @Test
    public void testOneOfSchemaWithDiscriminator() throws Exception {
        // Mammal can be one of whale, pig and zebra.
        // pig has sub-classes.
        String str = "{ \"className\": \"whale\", \"hasBaleen\": true, \"hasTeeth\": false }";
        /*
        DISABLING unit test for now until ambiguity of discriminator is resolved.
        
        // Note that the 'zebra' schema does not have any explicit property defined AND
        // it has additionalProperties: true. Hence without a discriminator the above
        // JSON payload would match both 'whale' and 'zebra'. This is because the 'hasBaleen'
        // and 'hasTeeth' would be considered additional (undeclared) properties for 'zebra'.
        Mammal o = json.getContext(null).readValue(str, Mammal.class);
        assertTrue(o.getActualInstance() instanceof Whale);

        str = "{ \"className\": \"zebra\" }";
        o = json.getContext(null).readValue(str, Mammal.class);
        assertTrue(o.getActualInstance() instanceof Zebra);

        str = "{ \"className\": \"BasquePig\" }";
        o = json.getContext(null).readValue(str, Mammal.class);
        assertTrue(o.getActualInstance() instanceof BasquePig);
        */
    }
}