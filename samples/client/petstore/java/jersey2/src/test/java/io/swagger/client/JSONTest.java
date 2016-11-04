package io.swagger.client;

import io.swagger.client.model.Order;
import org.junit.Before;
import org.junit.Test;
import org.threeten.bp.OffsetDateTime;
import org.threeten.bp.ZoneId;
import org.threeten.bp.format.DateTimeFormatter;

import static org.junit.Assert.*;


public class JSONTest {
    private JSON json = null;
    private Order order = null;

    @Before
    public void setup() {
        json = new ApiClient().getJSON();
        order = new Order();
    }

    @Test
    public void testDefaultDate() throws Exception {
        final DateTimeFormatter dateFormat = DateTimeFormatter.ISO_OFFSET_DATE_TIME;
        final String dateStr = "2015-11-07T14:11:05.267Z";
        order.setShipDate(dateFormat.parse(dateStr, OffsetDateTime.FROM));

        String str = json.getContext(null).writeValueAsString(order);
        Order o = json.getContext(null).readValue(str, Order.class);
        assertEquals(dateStr, dateFormat.format(o.getShipDate()));
    }

    @Test
    public void testCustomDate() throws Exception {
        final DateTimeFormatter dateFormat = DateTimeFormatter.ISO_OFFSET_DATE_TIME.withZone(ZoneId.of("Etc/GMT+2"));
        final String dateStr = "2015-11-07T14:11:05-02:00";
        order.setShipDate(dateFormat.parse(dateStr, OffsetDateTime.FROM));

        String str = json.getContext(null).writeValueAsString(order);
        Order o = json.getContext(null).readValue(str, Order.class);
        assertEquals(dateStr, dateFormat.format(o.getShipDate()));
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