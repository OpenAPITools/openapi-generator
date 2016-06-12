package io.swagger.client;

import com.google.gson.reflect.TypeToken;

import io.swagger.client.model.Order;

import java.lang.Exception;
import java.lang.reflect.Type;

import org.joda.time.DateTimeZone;
import org.joda.time.format.DateTimeFormatter;
import org.joda.time.format.ISODateTimeFormat;
import org.junit.*;
import static org.junit.Assert.*;

public class JSONTest {
    ApiClient apiClient = null;
    JSON json = null;
    Order order = null;

    @Before
    public void setup() {
        apiClient = new ApiClient();
        json = new JSON(apiClient);
        order = new Order();
    }

    @Test
    public void testDefaultDate() throws Exception {
        final DateTimeFormatter datetimeFormat = ISODateTimeFormat.dateTime().withZone(DateTimeZone.UTC);
        final String dateStr = "2015-11-07T14:11:05.267Z";
        order.setShipDate(datetimeFormat.parseDateTime(dateStr));

        String str = json.serialize(order);
        Type type = new TypeToken<Order>() { }.getType();
        Order o = json.deserialize(str, type);
        assertEquals(dateStr, datetimeFormat.print(o.getShipDate()));
    }

    @Test
    public void testCustomDate() throws Exception {
        final DateTimeFormatter datetimeFormat = ISODateTimeFormat.dateTimeNoMillis().withZone(DateTimeZone.forID("Etc/GMT+2"));
        final String dateStr = "2015-11-07T14:11:05-02:00";
        order.setShipDate(datetimeFormat.parseDateTime(dateStr));

        String str = json.serialize(order);
        Type type = new TypeToken<Order>() { }.getType();
        Order o = json.deserialize(str, type);
        assertEquals(dateStr, datetimeFormat.print(o.getShipDate()));
    }
}