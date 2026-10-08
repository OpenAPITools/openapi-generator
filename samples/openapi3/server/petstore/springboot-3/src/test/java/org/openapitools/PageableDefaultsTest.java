package org.openapitools;

import org.junit.jupiter.api.Test;
import org.springframework.core.MethodParameter;
import org.springframework.data.domain.Pageable;
import org.springframework.data.web.PageableDefault;
import org.springframework.data.web.PageableHandlerMethodArgumentResolver;
import org.springframework.mock.web.MockHttpServletRequest;
import org.springframework.web.context.request.ServletWebRequest;

import static org.assertj.core.api.Assertions.assertThat;

/**
 * Isolates Spring's resolver semantics using handwritten annotations.
 * Generator output is covered by SpringPageableOptionsTest and the Java/Kotlin
 * sort-validation samples, which resolve requests against generated interfaces.
 */
class PageableDefaultsTest {

    static class Endpoints {
        void unconverted(@PageableDefault(page = 1, size = 20) Pageable pageable) {
        }

        void converted(@PageableDefault(page = 0, size = 20) Pageable pageable) {
        }

        void defaultsDisabled(Pageable pageable) {
        }
    }

    private Pageable resolve(String method, String page) throws Exception {
        PageableHandlerMethodArgumentResolver resolver = new PageableHandlerMethodArgumentResolver();
        resolver.setOneIndexedParameters(true);
        MockHttpServletRequest request = new MockHttpServletRequest();
        if (page != null) {
            request.setParameter("page", page);
        }
        MethodParameter parameter = new MethodParameter(Endpoints.class.getDeclaredMethod(method, Pageable.class), 0);
        return resolver.resolveArgument(parameter, null, new ServletWebRequest(request), null);
    }

    @Test
    void unconvertedSpecDefaultSelectsSecondPageWhenPageIsOmitted() throws Exception {
        assertThat(resolve("unconverted", null).getPageNumber()).isEqualTo(1);
    }

    @Test
    void convertedDefaultSelectsFirstPageWhenPageIsOmitted() throws Exception {
        Pageable pageable = resolve("converted", null);
        assertThat(pageable.getPageNumber()).isZero();
        assertThat(pageable.getPageSize()).isEqualTo(20);
    }

    @Test
    void explicitOneBasedPageIsResolvedIndependentlyOfAnnotationDefault() throws Exception {
        assertThat(resolve("converted", "1").getPageNumber()).isZero();
        assertThat(resolve("converted", "2").getPageNumber()).isEqualTo(1);
    }

    @Test
    void disablingGeneratedDefaultsUsesSpringFallback() throws Exception {
        Pageable pageable = resolve("defaultsDisabled", null);
        assertThat(pageable.getPageNumber()).isZero();
        assertThat(pageable.getPageSize()).isEqualTo(20);
    }
}
