package io.swagger.client;

import static org.junit.Assert.*;
import org.junit.*;

public class ApiInvokerTest {
  @Test
  public void testSelectHeaderAccept() {
    String[] accepts = { "APPLICATION/JSON", "APPLICATION/XML" };
    assertEquals("application/json", ApiInvoker.selectHeaderAccept(accepts));

    accepts = new String[] { "application/json", "application/xml" };
    assertEquals("application/json", ApiInvoker.selectHeaderAccept(accepts));

    accepts = new String[] { "application/xml", "application/json" };
    assertEquals("application/json", ApiInvoker.selectHeaderAccept(accepts));

    accepts = new String[] { "text/plain", "application/xml" };
    assertEquals("text/plain,application/xml", ApiInvoker.selectHeaderAccept(accepts));

    accepts = new String[] { };
    assertEquals("application/json", ApiInvoker.selectHeaderAccept(accepts));
  }

  @Test
  public void testSelectHeaderContentType() {
    String[] contentTypes = { "APPLICATION/JSON", "APPLICATION/XML" };
    assertEquals("application/json", ApiInvoker.selectHeaderContentType(contentTypes));

    contentTypes = new String[] { "application/json", "application/xml" };
    assertEquals("application/json", ApiInvoker.selectHeaderContentType(contentTypes));

    contentTypes = new String[] { "application/xml", "application/json" };
    assertEquals("application/json", ApiInvoker.selectHeaderContentType(contentTypes));

    contentTypes = new String[] { "text/plain", "application/xml" };
    assertEquals("text/plain", ApiInvoker.selectHeaderContentType(contentTypes));

    contentTypes = new String[] { };
    assertEquals("application/json", ApiInvoker.selectHeaderContentType(contentTypes));
  }
}
