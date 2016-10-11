package io.swagger.client;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.ser.std.SqlDateSerializer;
import io.swagger.client.model.Order;

import java.lang.Exception;
import java.sql.Date;

import org.joda.time.DateTimeZone;
import org.joda.time.format.DateTimeFormatter;
import org.joda.time.format.ISODateTimeFormat;
import org.junit.*;
import static org.junit.Assert.*;


public class JSONTest {
    JSON json = null;
    Order order = null;

    @Before
    public void setup() {
        json = new ApiClient().getJSON();
        order = new Order();
    }

    @Test
    public void testDefaultDate() throws Exception {
        final DateTimeFormatter dateFormat = ISODateTimeFormat.dateTime();
        final String dateStr = "2015-11-07T14:11:05.267Z";
        order.setShipDate(dateFormat.parseDateTime(dateStr));

        String str = json.getContext(null).writeValueAsString(order);
        Order o = json.getContext(null).readValue(str, Order.class);
        assertEquals(dateStr, dateFormat.print(o.getShipDate()));
    }

    @Test
    public void testCustomDate() throws Exception {
        final DateTimeFormatter dateFormat = ISODateTimeFormat.dateTimeNoMillis().withZone(DateTimeZone.forID("Etc/GMT+2"));
        final String dateStr = "2015-11-07T14:11:05-02:00";
        order.setShipDate(dateFormat.parseDateTime(dateStr));

        String str = json.getContext(null).writeValueAsString(order);
        Order o = json.getContext(null).readValue(str, Order.class);
        assertEquals(dateStr, dateFormat.print(o.getShipDate()));
    }

    @Test
    public void testSqlDateSerialization() throws Exception {
        String str = json.getContext(null).writeValueAsString(new java.sql.Date(10));
        assertEquals("\"1970-01-01\"", str);
    }

    @Test
    public void testSqlDateDeserialization() throws Exception {
        final String str = "1970-01-01";
        java.sql.Date date = json.getContext(null).readValue("\"" + str + "\"", java.sql.Date.class);
        assertEquals(date.toString(), str);
    }

}