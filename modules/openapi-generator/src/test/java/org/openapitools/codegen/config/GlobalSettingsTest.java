package org.openapitools.codegen.config;

import ch.qos.logback.classic.Level;
import ch.qos.logback.classic.Logger;

import java.util.Properties;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatNoException;

import org.junit.jupiter.api.Disabled;
import org.slf4j.LoggerFactory;
import org.testng.annotations.BeforeClass;
import org.testng.annotations.Test;

/**
 * Test class for {@link GlobalSettings}
 *
 * @author Edoardo Patti
 */
public class GlobalSettingsTest {

    @BeforeClass
    public void setUp() {
        ((Logger) LoggerFactory.getLogger(GlobalSettings.class)).setLevel(Level.DEBUG);
        Properties props = new Properties(2);
        props.put("test1", 789);
        props.put(345, "test2");
        System.getProperties().putAll(props);
    }

    @Test @Disabled
    // comment out the following tests as it generates false alarms from time to time
    // also using system property will eventually be decommissioned
    public void testNonStringSystemProperties() {
        assertThat(GlobalSettings.getProperty("345")).isEqualTo("test2");
        assertThat(GlobalSettings.getProperty("test1")).isEqualTo("789");
        assertThatNoException().isThrownBy(GlobalSettings::log);
    }

}
