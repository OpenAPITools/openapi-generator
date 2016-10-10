package io.swagger.client;

import com.google.gson.reflect.TypeToken;

import io.swagger.client.model.Order;

import java.lang.Exception;
import java.lang.reflect.Type;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.Calendar;
import java.util.Date;
import java.util.GregorianCalendar;
import java.util.TimeZone;

import org.joda.time.DateTime;
import org.joda.time.DateTimeZone;
import org.joda.time.LocalDate;
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
        json = apiClient.getJSON();
        order = new Order();
    }

    @Test
    public void testSqlDateTypeAdapter() {
        final String str = "\"2015-11-07\"";
        final java.sql.Date date = java.sql.Date.valueOf("2015-11-07");

        assertEquals(str, json.serialize(date));
        assertEquals(json.deserialize(str, java.sql.Date.class), date);
        assertEquals(json.deserialize("\"2015-11-07T03:49:09.356+00:00\"", java.sql.Date.class).toString(), date.toString());

        // custom date format: without day
        DateFormat format = new SimpleDateFormat("yyyy-MM");
        apiClient.setSqlDateFormat(format);
        String dateStr = "\"2015-11\"";
        assertEquals(dateStr, json.serialize(json.deserialize("\"2015-11-07T03:49:09Z\"", java.sql.Date.class)));
        assertEquals(dateStr, json.serialize(json.deserialize("\"2015-11\"", java.sql.Date.class)));
    }

    @Test
    public void testDateTypeAdapter() {
        Calendar cal = new GregorianCalendar(2015, 10, 7, 3, 49, 9);
        cal.setTimeZone(TimeZone.getTimeZone("UTC"));

        assertEquals(json.deserialize("\"2015-11-07T05:49:09+02\"", Date.class), cal.getTime());

        cal.set(Calendar.MILLISECOND, 300);
        assertEquals(json.deserialize("\"2015-11-07T03:49:09.3Z\"", Date.class), cal.getTime());

        cal.set(Calendar.MILLISECOND, 356);
        Date date = cal.getTime();

        final String utcDate = "\"2015-11-07T03:49:09.356Z\"";
        assertEquals(json.deserialize(utcDate, Date.class), date);
        assertEquals(json.deserialize("\"2015-11-07T03:49:09.356+00:00\"", Date.class), date);
        assertEquals(json.deserialize("\"2015-11-07T05:49:09.356+02:00\"", Date.class), date);
        assertEquals(json.deserialize("\"2015-11-07T02:49:09.356-01:00\"", Date.class), date);
        assertEquals(json.deserialize("\"2015-11-07T03:49:09.356Z\"", Date.class), date);
        assertEquals(json.deserialize("\"2015-11-07T03:49:09.356+00\"", Date.class), date);
        assertEquals(json.deserialize("\"2015-11-07T02:49:09.356-0100\"", Date.class), date);
        assertEquals(json.deserialize("\"2015-11-07T03:49:09.356456789Z\"", Date.class), date);

        assertEquals(utcDate, json.serialize(date));

        // custom datetime format: without milli-seconds, custom time zone
        DateFormat format = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ssXXX");
        format.setTimeZone(TimeZone.getTimeZone("GMT+10"));
        apiClient.setDateFormat(format);

        String dateStr = "\"2015-11-07T13:49:09+10:00\"";
        assertEquals(dateStr, json.serialize(json.deserialize("\"2015-11-07T03:49:09+00:00\"", Date.class)));
        assertEquals(dateStr, json.serialize(json.deserialize("\"2015-11-07T03:49:09Z\"", Date.class)));
        assertEquals(dateStr, json.serialize(json.deserialize("\"2015-11-07T00:49:09-03:00\"", Date.class)));

        try {
            // invalid time zone format
            json.deserialize("\"2015-11-07T03:49:09+00\"", Date.class);
            fail("json parsing should fail");
        } catch (RuntimeException e) {
            // OK
        }
        try {
            // unexpected miliseconds
            json.deserialize("\"2015-11-07T03:49:09.000Z\"", Date.class);
            fail("json parsing should fail");
        } catch (RuntimeException e) {
            // OK
        }

    }

    @Test
    public void testDateTimeTypeAdapter() {
        final String str = "\"2016-09-09T08:02:03.123-03:00\"";
        DateTime date = new DateTime(2016, 9, 9, 8, 2, 3, 123, DateTimeZone.forID("Etc/GMT+3"));

        assertEquals(str, json.serialize(date));
        //Use toString() instead of isEqual to verify that the offset is preserved
        assertEquals(json.deserialize(str, DateTime.class).toString(), date.toString());
    }

    @Test
    public void testLocalDateTypeAdapter() {
        final String str = "\"2016-09-09\"";
        final LocalDate date = new LocalDate(2016, 9, 9);

        assertEquals(str, json.serialize(date));
        assertEquals(json.deserialize(str, LocalDate.class), date);
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