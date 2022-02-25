package org.openapitools.codegen.utils;

import com.github.benmanes.caffeine.cache.Cache;
import com.github.benmanes.caffeine.cache.Caffeine;
import com.github.benmanes.caffeine.cache.Ticker;
import org.openapitools.codegen.config.GlobalSettings;
import org.slf4j.Logger;
import org.slf4j.Marker;
import org.slf4j.MarkerFactory;
import org.slf4j.ext.LoggerWrapper;

import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicInteger;

/**
 * Provides calling code a way to log important messages only once, regardless of how many times the invocation has occurred.
 * This can be used, for instance, to log a warning like "One or more schemas aren't declared" without logging that message
 * for every time the schema is mentioned in a document.
 *
 * This implementation currently only supports single-argument string literal log methods (e.g. {@link Logger#debug(String)}).
 */
@SuppressWarnings("FieldCanBeLocal")
public class OnceLogger extends LoggerWrapper {
    /**
     * Allow advanced users to modify cache size of the OnceLogger (more for performance tuning in hosted environments)
     */
    static final String CACHE_SIZE_PROPERTY = "org.openapitools.codegen.utils.oncelogger.cachesize";

    /**
     * Allow advanced users to disable the OnceLogger (more for performance tuning in hosted environments).
     * This is really only useful or necessary if this implementation causes issues.
     */
    static final String ENABLE_ONCE_LOGGER_PROPERTY = "org.openapitools.codegen.utils.oncelogger.enabled";

    /**
     * Allow advanced users to modify cache expiration of the OnceLogger (more for performance tuning in hosted environments)
     */
    static final String EXPIRY_PROPERTY = "org.openapitools.codegen.utils.oncelogger.expiry";

    /**
     * Internal message cache for logger decorated with the onceler.
     */
    static Cache<String, AtomicInteger> messageCountCache;

    /**
     * The fully qualified class name of the <b>logger instance</b>,
     * typically the logger class, logger bridge or a logger wrapper.
     */
    private static final String FQCN = OnceLogger.class.getName();

    /**
     * Gets the marker instance. This can be used by supported log implementations to filter/manage logs coming from
     * this implementation differently than others (i.e. make them stand out since they're to be logged once).
     */
    private static final Marker MARKER = MarkerFactory.getMarker("ONCE");

    /**
     * The allowed size of the cache.
     */
    private static int maxCacheSize = Integer.parseInt(GlobalSettings.getProperty(CACHE_SIZE_PROPERTY, "200"));

    /**
     * The millis to expire a cached log message.
     */
    private static int expireMillis = Integer.parseInt(GlobalSettings.getProperty(EXPIRY_PROPERTY, "2000"));

    /**
     * The number of allowed repetitions.
     */
    private static int maxRepetitions = 1;

    OnceLogger(Logger logger) {
        this(logger, FQCN);
    }

    OnceLogger(Logger logger, String fqcn) {
        super(logger, fqcn);
    }

    static {
        caffeineCache(Ticker.systemTicker(), expireMillis);
    }

    static void caffeineCache(Ticker ticker, int expireMillis) {
        // Initializes a cache which holds an atomic counter of log message instances.
        // The intent is to debounce log messages such that they occur at most [maxRepetitions] per [expireMillis].
        messageCountCache = Caffeine.newBuilder()
                .maximumSize(maxCacheSize)
                .expireAfterWrite(expireMillis, TimeUnit.MILLISECONDS)
                .ticker(ticker)
                .build();
    }

    public static Logger once(Logger logger) {
        try {
            if (Boolean.parseBoolean(GlobalSettings.getProperty(ENABLE_ONCE_LOGGER_PROPERTY, "true"))) {
                return new OnceLogger(logger);
            }
        } catch (Exception ex) {
            logger.warn("Unable to wrap logger instance in OnceLogger. Falling back to non-decorated implementation, which may be noisy.");
        }
        return logger;
    }

    /**
     * Delegate to the appropriate method of the underlying logger.
     *
     * @param msg The log message.
     */
    @Override
    public void trace(String msg) {
        if (!isTraceEnabled() || !isTraceEnabled(MARKER)) return;

        if (shouldLog(msg)) super.trace(MARKER, msg);
    }

    @SuppressWarnings("ConstantConditions")
    private boolean shouldLog(final String msg) {
        AtomicInteger counter = messageCountCache.get(msg, i -> new AtomicInteger(0));
        return counter.incrementAndGet() <= maxRepetitions;
    }

    /**
     * Delegate to the appropriate method of the underlying logger.
     *
     * @param msg The log message.
     */
    @Override
    public void debug(String msg) {
        if (!isDebugEnabled() || !isDebugEnabled(MARKER)) return;

        if (shouldLog(msg)) super.debug(MARKER, msg);
    }

    /**
     * Delegate to the appropriate method of the underlying logger.
     *
     * @param msg The log message.
     */
    @Override
    public void info(String msg) {
        if (!isInfoEnabled() || !isInfoEnabled(MARKER)) return;

        if (shouldLog(msg)) super.info(MARKER, msg);
    }

    /**
     * Delegate to the appropriate method of the underlying logger.
     *
     * @param msg The log message.
     */
    @Override
    public void warn(String msg) {
        if (!isWarnEnabled() || !isWarnEnabled(MARKER)) return;

        if (shouldLog(msg)) super.warn(MARKER, msg);
    }

    /**
     * Delegate to the appropriate method of the underlying logger.
     *
     * Use this method sparingly. If you're limiting error messages, ask yourself
     * whether your log fits better as a warning.
     *
     * @param msg The log message.
     */
    @Override
    public void error(String msg) {
        if (!isErrorEnabled() || !isErrorEnabled(MARKER)) return;

        if (shouldLog(msg)) super.error(MARKER, msg);
    }
}
