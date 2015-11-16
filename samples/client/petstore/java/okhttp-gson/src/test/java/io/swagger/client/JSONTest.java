package io.swagger.client;

import com.google.gson.reflect.TypeToken;

import io.swagger.client.model.Order;

import java.lang.Exception;
import java.lang.reflect.Type;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.*;

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
        final DateFormat datetimeFormat = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss.SSSXXX");
        datetimeFormat.setTimeZone(TimeZone.getTimeZone("UTC"));
        final String dateStr = "2015-11-07T14:11:05.267Z";
        order.setShipDate(datetimeFormat.parse(dateStr));

        String str = json.serialize(order);
        Type type = new TypeToken<Order>() { }.getType();
        Order o = json.deserialize(str, type);
        assertEquals(dateStr, datetimeFormat.format(o.getShipDate()));
    }

    @Test
    public void testCustomDate() throws Exception {
        final DateFormat datetimeFormat = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ssXXX");
        datetimeFormat.setTimeZone(TimeZone.getTimeZone("GMT-2"));
        final String dateStr = "2015-11-07T14:11:05-02:00";
        order.setShipDate(datetimeFormat.parse(dateStr));

        apiClient.setDatetimeFormat(datetimeFormat);
        apiClient.setLenientDatetimeFormat(false);
        String str = json.serialize(order);
        Type type = new TypeToken<Order>() { }.getType();
        Order o = json.deserialize(str, type);
        assertEquals(dateStr, datetimeFormat.format(o.getShipDate()));
    }
}