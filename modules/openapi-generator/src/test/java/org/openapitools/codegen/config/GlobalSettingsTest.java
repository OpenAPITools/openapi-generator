package org.openapitools.codegen.config;

import ch.qos.logback.classic.Level;
import ch.qos.logback.classic.Logger;
import java.util.Properties;
import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatNoException;
import org.slf4j.LoggerFactory;
import org.testng.annotations.BeforeClass;
import org.testng.annotations.Test;

/**
 * Test class for {@link GlobalSettings}
 * @author Edoardo Patti
 */
public class GlobalSettingsTest {

  private static final Object OBJECT = new Object();

  @BeforeClass
  public void setUp() {
    ((Logger) LoggerFactory.getLogger(GlobalSettings.class)).setLevel(Level.DEBUG);
    Properties props = new Properties(2);
    props.put("test1", OBJECT);
    props.put(OBJECT, "test2");
    System.getProperties().putAll(props);
  }

  @Test
  public void testNonStringSystemProperties() {
    assertThat(GlobalSettings.getProperty(OBJECT.toString())).isNotNull();
    assertThat(GlobalSettings.getProperty("test1")).isNotNull();
    assertThatNoException().isThrownBy(GlobalSettings::log);
  }

}