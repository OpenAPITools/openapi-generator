package org.openapitools.codegen.utils;

import org.mockito.Mockito;
import org.openapitools.codegen.config.GlobalSettings;
import org.slf4j.Logger;
import org.slf4j.Marker;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.Test;

import static org.mockito.Mockito.*;
import static org.openapitools.codegen.utils.OnceLogger.*;
import static org.testng.Assert.*;

@SuppressWarnings("SameParameterValue")
public class OnceLoggerTest {
    private Logger mockLogger = Mockito.mock(Logger.class);

    @AfterMethod
    public void afterEach() {
        OnceLogger.messageCountCache.asMap().clear();
    }

    @BeforeMethod
    public void beforeEach() {
        Mockito.reset(mockLogger);
    }

    @Test
    public void onceLogsMessagesOnceWithinTimeLimit() throws InterruptedException {
        String originalSetting = GlobalSettings.getProperty(EXPIRY_PROPERTY);
        long expiry = Long.parseLong(originalSetting);
        String message = "onceLogsMessagesOnceWithinTimeLimit";

        Logger instance = once(mockLogger);
        when(mockLogger.isWarnEnabled()).thenReturn(true);
        when(mockLogger.isWarnEnabled(any(Marker.class))).thenReturn(true);

        assertEquals(expiry, 1000L, "Expected expiry to be set to 1000 in POM's surefire configuration.");

        try {
            for (int i = 0; i < 50; i++) {
                instance.warn(message);
            }

            Thread.sleep(expiry + 150L);

            for (int i = 0; i < 50; i++) {
                instance.warn(message);
            }
        } finally {
            verify(mockLogger, times(2)).warn(any(Marker.class), same(message));
        }
    }

    @Test
    public void onceLogsOneMessageByDefault() {
        String message = "onceLogsOneMessageByDefault";
        Logger instance = once(mockLogger);
        when(mockLogger.isWarnEnabled()).thenReturn(true);
        when(mockLogger.isWarnEnabled(any(Marker.class))).thenReturn(true);

        for (int i = 0; i < 50; i++) {
            instance.warn(message);
        }

        verify(mockLogger, times(1)).warn(any(Marker.class), same(message));
    }

    @Test
    public void onceReturnsDecoratedLogger() {
        Logger instance = once(mockLogger);
        assertTrue(instance instanceof OnceLogger);
    }

    @Test
    public void onceReturnsOriginalLoggerWhenDisabled() {
        String originalSetting = GlobalSettings.getProperty(ENABLE_ONCE_LOGGER_PROPERTY);
        try {
            GlobalSettings.setProperty(ENABLE_ONCE_LOGGER_PROPERTY, "false");
            Logger instance = once(mockLogger);
            assertFalse(instance instanceof OnceLogger);
        } finally {
            resetGlobalProperty(ENABLE_ONCE_LOGGER_PROPERTY, originalSetting);
        }
    }

    private void resetGlobalProperty(String key, String value) {
        if (value != null)
            GlobalSettings.setProperty(key, value);
        else
            GlobalSettings.clearProperty(key);
    }
}