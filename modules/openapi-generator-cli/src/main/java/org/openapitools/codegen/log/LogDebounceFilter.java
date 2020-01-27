package org.openapitools.codegen.log;

import ch.qos.logback.classic.Level;
import ch.qos.logback.classic.Logger;
import ch.qos.logback.classic.turbo.DuplicateMessageFilter;
import ch.qos.logback.classic.turbo.TurboFilter;
import ch.qos.logback.core.spi.FilterReply;
import com.github.benmanes.caffeine.cache.Cache;
import com.github.benmanes.caffeine.cache.Caffeine;
import org.slf4j.Marker;

import java.util.concurrent.TimeUnit;

/**
 * Modeled after {@link DuplicateMessageFilter}, this uses a Caffeine cache for automatic expiration.
 */
@SuppressWarnings("unused")
public class LogDebounceFilter extends TurboFilter {

    /**
     * The default cache size.
     */
    public static final int DEFAULT_CACHE_SIZE = 100;

    /**
     * The default number of allows repetitions.
     */
    public static final int DEFAULT_ALLOWED_REPETITIONS = 5;

    /**
     * The default millis to expire a cached log message.
     */
    public static final int DEFAULT_EXPIRE_MILLIS = 2000;

    private int maxCacheSize = DEFAULT_CACHE_SIZE;
    private int expireMillis = DEFAULT_EXPIRE_MILLIS;
    private int maxRepetitions = DEFAULT_ALLOWED_REPETITIONS;
    private Cache<String, Integer> messageCountCache;

    @Override
    public FilterReply decide(Marker marker, Logger logger, Level level, String format, Object[] params, Throwable t) {
        if (!isStarted()) {
            return FilterReply.NEUTRAL;
        }

        Integer count = messageCountCache.get(format, i -> 0);
        count += 1;
        messageCountCache.put(format, count);

        if (count <= maxRepetitions) {
            return FilterReply.NEUTRAL;
        }

        return FilterReply.DENY;
    }

    @Override
    public void start() {
        messageCountCache = Caffeine.newBuilder()
                .maximumSize(maxCacheSize)
                .expireAfterWrite(expireMillis, TimeUnit.MILLISECONDS)
                .build();
        super.start();
    }

    @Override
    public void stop() {
        messageCountCache.cleanUp();
        messageCountCache = null;
        super.stop();
    }

    public int getExpireMillis() {
        return expireMillis;
    }

    public void setExpireMillis(int expireMillis) {
        this.expireMillis = Math.max(250, expireMillis);
    }

    public int getMaxRepetitions() {
        return maxRepetitions;
    }

    public void setMaxRepetitions(int maxRepetitions) {
        this.maxRepetitions = maxRepetitions;
    }

    public int getMaxCacheSize() {
        return maxCacheSize;
    }

    public void setMaxCacheSize(int maxCacheSize) {
        this.maxCacheSize = Math.max(100, maxCacheSize);
    }
}
