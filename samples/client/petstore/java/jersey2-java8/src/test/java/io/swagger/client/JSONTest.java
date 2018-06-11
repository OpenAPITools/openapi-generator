package io.swagger.client;

import io.swagger.client.model.Order;

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
}