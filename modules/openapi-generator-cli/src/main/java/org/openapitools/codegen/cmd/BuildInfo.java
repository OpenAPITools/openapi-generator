package org.openapitools.codegen.cmd;

import java.io.IOException;
import java.io.InputStream;
import java.time.DateTimeException;
import java.time.OffsetDateTime;
import java.time.format.DateTimeFormatter;
import java.time.format.DateTimeParseException;
import java.util.Locale;
import java.util.Properties;

import static org.openapitools.codegen.Constants.*;

/**
 * Presents build-time information
 */
@SuppressWarnings({"java:S108"})
public class BuildInfo {
    private static final String VERSION_PLACEHOLDER = "${project.version}";
    private static final String UNSET = "unset";
    private static final String UNKNOWN = "unknown";

    private static final Properties properties = new Properties();

    static {
        try (InputStream is = BuildInfo.class.getResourceAsStream("/version.properties")) {
            if (is != null) {
                Properties versionProps = new Properties();
                versionProps.load(is);
                properties.putAll(versionProps);
            }
        } catch (IOException ignored) {
        }
        try (InputStream is = BuildInfo.class.getResourceAsStream("/openapi-generator-git.properties")) {
            if (is != null) {
                Properties gitProps = new Properties();
                gitProps.load(is);
                properties.putAll(gitProps);
            }
        } catch (IOException ignored) {
        }
    }

    /**
     * Gets the version of the toolset.
     *
     * @return A semver string
     */
    public String getVersion() {
        String version = (String) properties.getOrDefault("version", UNKNOWN);
        if (VERSION_PLACEHOLDER.equals(version)) {
            return UNSET;
        } else {
            return version;
        }
    }

    /**
     * Gets the git commit SHA1 hash. Useful for differentiating between SNAPSHOT builds.
     *
     * @return A short git SHA
     */
    public String getSha() {
        return (String) properties.getOrDefault("git.commit.id.abbrev", UNKNOWN);
    }

    /**
     * Gets the time when this tool was built.
     *
     * @return The time as {@link OffsetDateTime}, or {@link OffsetDateTime#MIN} if metadata cannot be parsed.
     */
    public OffsetDateTime getBuildTime() {
        try {
            String time = (String) properties.getOrDefault("git.build.time", "");
            return OffsetDateTime.parse(time, DateTimeFormatter.ofPattern("yyyy-MM-dd'T'HH:mm:ssZ", Locale.ROOT));
        } catch (DateTimeParseException e) {
            return OffsetDateTime.MIN;
        }
    }

    /**
     * Gets the full version display text, as one would expect from a '--version' CLI option
     *
     * @return Human-readable version display information
     */
    public String versionDisplayText() {
        StringBuilder sb = new StringBuilder(CLI_NAME);
        sb.append(" ").append(this.getVersion()).append(System.lineSeparator());
        sb.append("  commit : ").append(this.getSha()).append(System.lineSeparator());
        sb.append("  built  : ");
        try {
            sb.append(this.getBuildTime().format(DateTimeFormatter.ISO_OFFSET_DATE_TIME));
        } catch (DateTimeException e) {
            sb.append(UNKNOWN);
        }
        sb.append(System.lineSeparator());
        sb.append("  source : ").append(GIT_REPO).append(System.lineSeparator());
        sb.append("  docs   : ").append(SITE).append(System.lineSeparator());
        return sb.toString();
    }
}
