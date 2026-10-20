package org.openapitools.codegen;

import org.testng.annotations.Test;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;
import java.util.Locale;

import static org.assertj.core.api.Assertions.assertThatCode;
import static org.assertj.core.api.Assertions.assertThatThrownBy;

@SuppressWarnings("NewClassNamingConvention")
public class TestUtilsTest {

    // forbiddenapis check will fail if we don't explicitly define localization when using String.format
    static final Locale L = null;

    public static class validatePomXmlFiles {

        static final String POM_SCAFFOLD = "<project>%s<dependencies>%s</dependencies></project>";
        static final String POM_PROJECT_INFO = "<name>foo</name><artifactId>foo.bar</artifactId><groupId>foobar</groupId><version>13.12</version>";

        @Test
        void doesNotThrow_ifNotPom_doesNotExist() {
            final var vaporFile = newTempDir().resolve("other.xml").toFile();

            assertThatCode(() -> TestUtils.validatePomXmlFiles(List.of(vaporFile)))
                    .doesNotThrowAnyException();
        }

        @Test
        void throwsRuntimeException_ifPomDoesNotExist() {
            final var vaporBom = newTempDir().resolve("pom.xml").toFile();

            assertThatThrownBy(() -> TestUtils.validatePomXmlFiles(List.of(vaporBom)))
                    .isExactlyInstanceOf(RuntimeException.class);
        }

        @Test
        void throwsRuntimeException_ifXmlIsJson() {
            Path testFile = newPomXmlFile("{\"not_xml\": 12345}");

            assertThatThrownBy(() -> TestUtils.validatePomXmlFiles(List.of(testFile.toFile())))
                    .isExactlyInstanceOf(RuntimeException.class);
        }

        @Test
        void throwsRuntimeException_ifXmlIsInvalid() {
            final Path testFile = newPomXmlFile("<IAmNotClosed>");

            assertThatThrownBy(() -> TestUtils.validatePomXmlFiles(List.of(testFile.toFile())))
                    .isExactlyInstanceOf(RuntimeException.class);
        }

        @Test
        void throwsAssertionError_ifNameTagIsMissing() {
            final Path testFile = newPomXmlFile(replaceTag("name", "", getMinimalValidPomContent()));

            assertThatThrownBy(() -> TestUtils.validatePomXmlFiles(List.of(testFile.toFile())))
                    .isExactlyInstanceOf(AssertionError.class);
        }

        @Test
        void doesNotThrow_ifNameIsEmpty() {
            final Path testFile = newPomXmlFile(
                    replaceTag("name", "<name></name>", getMinimalValidPomContent())
            );

            assertThatCode(() -> TestUtils.validatePomXmlFiles(List.of(testFile.toFile())))
                    .doesNotThrowAnyException();
        }

        @Test
        void throwsAssertionError_ifArtifactIdTagIsMissing() {
            final Path testFile = newPomXmlFile(replaceTag("artifactId", "", getMinimalValidPomContent()));

            assertThatThrownBy(() -> TestUtils.validatePomXmlFiles(List.of(testFile.toFile())))
                    .isExactlyInstanceOf(AssertionError.class);
        }

        @Test
        void doesNotThrow_ifArtifactIdIsEmpty() {
            final Path testFile = newPomXmlFile(
                    replaceTag("artifactId", "<artifactId></artifactId>", getMinimalValidPomContent())
            );

            assertThatCode(() -> TestUtils.validatePomXmlFiles(List.of(testFile.toFile())))
                    .doesNotThrowAnyException();
        }

        @Test
        void throwsAssertionError_ifGroupIdTagIsMissing() {
            final Path testFile = newPomXmlFile(replaceTag("groupId", "", getMinimalValidPomContent()));

            assertThatThrownBy(() -> TestUtils.validatePomXmlFiles(List.of(testFile.toFile())))
                    .isExactlyInstanceOf(AssertionError.class);
        }

        @Test
        void doesNotThrow_ifGroupIdIsEmpty() {
            final Path testFile = newPomXmlFile(
                    replaceTag("groupId", "<groupId></groupId>", getMinimalValidPomContent())
            );

            assertThatCode(() -> TestUtils.validatePomXmlFiles(List.of(testFile.toFile())))
                    .doesNotThrowAnyException();
        }

        @Test
        void throwsAssertionError_ifVersionTagIsMissing() {
            final Path testFile = newPomXmlFile(replaceTag("version", "", getMinimalValidPomContent()));

            assertThatThrownBy(() -> TestUtils.validatePomXmlFiles(List.of(testFile.toFile())))
                    .isExactlyInstanceOf(AssertionError.class);
        }

        @Test
        void doesNotThrow_ifVersionIsEmpty() {
            final Path testFile = newPomXmlFile(
                    replaceTag("version", "<version></version>", getMinimalValidPomContent())
            );

            assertThatCode(() -> TestUtils.validatePomXmlFiles(List.of(testFile.toFile())))
                    .doesNotThrowAnyException();
        }

        @Test
        void throwsAssertionError_ifZeroDependencies() {
            final Path testFile = newPomXmlFile(String.format(L, POM_SCAFFOLD, POM_PROJECT_INFO, ""));

            assertThatThrownBy(() -> TestUtils.validatePomXmlFiles(List.of(testFile.toFile())))
                    .isExactlyInstanceOf(AssertionError.class);
        }

        @Test
        void doesNotThrow_ifAtLeastOneDependency() {
            final Path testFile = newPomXmlFile(String.format(L, POM_SCAFFOLD, POM_PROJECT_INFO, "<dependency />"));

            assertThatCode(() -> TestUtils.validatePomXmlFiles(List.of(testFile.toFile())))
                    .doesNotThrowAnyException();
        }

        @Test
        void throwsAssertionError_withTwoValidPoms_whereSecondHasNoName() {
            final File testFile1 = newPomXmlFile(getMinimalValidPomContent()).toFile();
            final File testFile2 = newPomXmlFile(replaceTag("name", "", getMinimalValidPomContent())).toFile();

            assertThatThrownBy(() -> TestUtils.validatePomXmlFiles(List.of(testFile1, testFile2)))
                    .isExactlyInstanceOf(AssertionError.class);
        }

        @Test
        void doesNotThrow_withTwoValidPoms() {
            final File testFile1 = newPomXmlFile(getMinimalValidPomContent()).toFile();
            final File testFile2 = newPomXmlFile(getMinimalValidPomContent()).toFile();

            assertThatCode(() -> TestUtils.validatePomXmlFiles(List.of(testFile1, testFile2)))
                    .doesNotThrowAnyException();
        }

        private String replaceTag(String tagToReplace, String replaceWith, String xml) {
            return xml.replaceAll("<" + tagToReplace + ">[\\s\\S]*?</" + tagToReplace + ">", replaceWith);
        }

        private String getMinimalValidPomContent() {
            return String.format(L, POM_SCAFFOLD, POM_PROJECT_INFO, "<dependency />");
        }

        private Path newPomXmlFile(String content) {
            try {
                return Files.writeString(newTempDir().resolve("pom.xml"), content);
            } catch (IOException e) {
                throw new RuntimeException(e);
            }
        }

        private Path newTempDir() {
            final Path tempDir;
            try {
                tempDir = Files.createTempDirectory("test");
            } catch (IOException e) {
                throw new RuntimeException(e);
            }
            tempDir.toFile().deleteOnExit();
            return tempDir;
        }
    }
}