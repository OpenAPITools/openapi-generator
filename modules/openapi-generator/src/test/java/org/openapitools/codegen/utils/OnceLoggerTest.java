package org.openapitools.codegen.utils;

import com.google.common.testing.FakeTicker;
import org.mockito.Mockito;
import org.openapitools.codegen.config.GlobalSettings;
import org.slf4j.Logger;
import org.slf4j.Marker;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.BeforeTest;
import org.testng.annotations.Test;

import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicInteger;

import static org.mockito.Mockito.*;
import static org.openapitools.codegen.utils.OnceLogger.*;
import static org.testng.Assert.*;

@SuppressWarnings({"SameParameterValue", "UnstableApiUsage"})
public class OnceLoggerTest {
    private Logger mockLogger = Mockito.mock(Logger.class);
    private FakeTicker ticker = new FakeTicker();

    @BeforeTest
    public void setUp(){
        OnceLogger.caffeineCache(ticker::read, 1000);
    }

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
        String message = "onceLogsMessagesOnceWithinTimeLimit";
        Logger instance = once(mockLogger);
        when(mockLogger.isWarnEnabled()).thenReturn(true);
        when(mockLogger.isWarnEnabled(any(Marker.class))).thenReturn(true);

        for (int i = 0; i < 50; i++) {
            instance.warn(message);
        }

        AtomicInteger counter1 = messageCountCache.getIfPresent(message);
        assertNotNull(counter1);
        assertEquals(counter1.get(), 50);

        ticker.advance(1100L, TimeUnit.MILLISECONDS);

        AtomicInteger noCounter = messageCountCache.getIfPresent(message);
        assertNull(noCounter);
        verify(mockLogger, times(1)).warn(any(Marker.class), same(message));

        for (int i = 0; i < 50; i++) {
            instance.warn(message);
        }

        ticker.advance(5, TimeUnit.MILLISECONDS);

        AtomicInteger counter2 = messageCountCache.getIfPresent(message);

        assertNotNull(counter2);
        assertNotEquals(counter2, counter1);
        assertEquals(counter2.get(), 50);
        verify(mockLogger, times(2)).warn(any(Marker.class), same(message));
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