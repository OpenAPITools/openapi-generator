package org.openapitools.client;

import org.openapitools.client.model.Order;
import org.openapitools.client.model.SpecialModelName;

import java.lang.Exception;

import java.time.OffsetDateTime;
import java.time.ZoneId;
import java.time.format.DateTimeFormatter;
import org.junit.*;
import static org.junit.Assert.*;


public class JSONTest {
    JSON json = null;
    Order order = null;

    @Before
    public void setup() {
        json = new JSON();
        order = new Order();
    }

    @Test
    public void testDefaultDate() throws Exception {
        final DateTimeFormatter dateFormat = DateTimeFormatter.ISO_OFFSET_DATE_TIME;
        final String dateStr = "2015-11-07T14:11:05.267Z";
        order.setShipDate(dateFormat.parse(dateStr, OffsetDateTime::from));

        String str = json.getContext(null).writeValueAsString(order);
        Order o = json.getContext(null).readValue(str, Order.class);
        assertEquals(dateStr, dateFormat.format(o.getShipDate()));
    }

    @Test
    public void testCustomDate() throws Exception {
        final DateTimeFormatter dateFormat = DateTimeFormatter.ISO_OFFSET_DATE_TIME.withZone(ZoneId.of("Etc/GMT+2"));
        final String dateStr = "2015-11-07T14:11:05-02:00";
        order.setShipDate(dateFormat.parse(dateStr, OffsetDateTime::from));

        String str = json.getContext(null).writeValueAsString(order);
        Order o = json.getContext(null).readValue(str, Order.class);
        assertEquals(dateStr, dateFormat.format(o.getShipDate()));
    }

    /**
     * Validate a schema with special characters can be deserialized. 
     */
    @Test
    public void testSchemaWithSpecialCharacters() throws Exception {
        String str = "{ \"$special[property.name]\": 12345 }";
        // The name of the OpenAPI schema is '_special_model.name_'.
        // After sanitization rules are applied the name of the class is 'SpecialModelName'.
        // The class deserialization should be successful because
        // of the @JsonSubTypes annotation.
        SpecialModelName o = json.getContext(null).readValue(str, SpecialModelName.class);
        assertNotNull(o);
        assertEquals((long)12345, (long)o.get$SpecialPropertyName());
    }    
}