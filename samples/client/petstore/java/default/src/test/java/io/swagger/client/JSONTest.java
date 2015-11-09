package io.swagger.client;

import io.swagger.client.model.Order;

import java.lang.Exception;
import java.text.DateFormat;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.*;
import java.util.TimeZone;

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
        final DateFormat dateFormat = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss.SSSXXX");
        dateFormat.setTimeZone(TimeZone.getTimeZone("UTC"));
        final String dateStr = "2015-11-07T14:11:05.267Z";
        order.setShipDate(dateFormat.parse(dateStr));

        String str = json.serialize(order);
        TypeRef typeRef = new TypeRef<Order>() { };
        Order o = json.deserialize(str, typeRef);
        assertEquals(dateStr, dateFormat.format(o.getShipDate()));
    }

    @Test
    public void testCustomDate() throws Exception {
        final DateFormat dateFormat = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ssXXX");
        dateFormat.setTimeZone(TimeZone.getTimeZone("GMT-2"));
        final String dateStr = "2015-11-07T14:11:05-02:00";
        order.setShipDate(dateFormat.parse(dateStr));

        json.setDateFormat(dateFormat);
        String str = json.serialize(order);
        TypeRef typeRef = new TypeRef<Order>() { };
        Order o = json.deserialize(str, typeRef);
        assertEquals(dateStr, dateFormat.format(o.getShipDate()));
    }
}