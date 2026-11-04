package org.openapitools.model;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.context.annotation.Import;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.setup.MockMvcBuilders;
import org.springframework.web.context.WebApplicationContext;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RestController;

import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

@SpringBootTest
@Import(FilterBindingTest.EchoController.class)
class FilterBindingTest {

    /** Returns the query object the way the generated API methods receive it. */
    @RestController
    static class EchoController {
        @GetMapping("/test/filter")
        Filter echo(@jakarta.validation.Valid Filter filter) {
            return filter;
        }
    }

    @Autowired
    private WebApplicationContext context;

    private MockMvc mvc;

    @BeforeEach
    void setUp() {
        mvc = MockMvcBuilders.webAppContextSetup(context).build();
    }

    @Test
    void aRepeatedParameterFillsTheList() throws Exception {
        mvc.perform(get("/test/filter").param("tags", "a", "b"))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.tags[0]").value("a"))
                .andExpect(jsonPath("$.tags[1]").value("b"));
    }

    @Test
    void anIndexedParameterFillsTheList() throws Exception {
        mvc.perform(get("/test/filter").param("tags[0]", "a"))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.tags[0]").value("a"));
    }

    @Test
    void aSanitisedDocumentedNameIsBound() throws Exception {
        mvc.perform(get("/test/filter").param("sort-order", "x"))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$['sort-order']").value("x"));
    }

    @Test
    void anAbsentLimitIsTheDefault() throws Exception {
        mvc.perform(get("/test/filter"))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.limit").value(10));
    }

    @Test
    void anEmptyLimitIsTheDefault() throws Exception {
        mvc.perform(get("/test/filter").param("limit", ""))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.limit").value(10));
    }

    @Test
    void aLimitThatIsNotANumberIsABadRequest() throws Exception {
        mvc.perform(get("/test/filter").param("limit", "abc"))
                .andExpect(status().isBadRequest());
    }

    @Test
    void aNestedRecordIsBoundByTheDottedName() throws Exception {
        mvc.perform(get("/test/filter").param("range.from", "1"))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.range.from").value(1));
    }
}
