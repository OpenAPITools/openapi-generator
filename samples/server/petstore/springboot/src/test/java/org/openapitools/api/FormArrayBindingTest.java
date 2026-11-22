package org.openapitools.api;

import java.util.List;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.setup.MockMvcBuilders;
import org.springframework.web.bind.annotation.RestController;

import static org.assertj.core.api.Assertions.assertThat;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

class FormArrayBindingTest {

    private FormArrayController controller;
    private MockMvc mockMvc;

    @BeforeEach
    void setUp() {
        controller = new FormArrayController();
        mockMvc = MockMvcBuilders.standaloneSetup(controller).build();
    }

    @Test
    void repeatedFormFieldsBindToAList() throws Exception {
        mockMvc.perform(get(FakeApi.PATH_TEST_ENUM_PARAMETERS)
                        .contentType(MediaType.APPLICATION_FORM_URLENCODED)
                        .param("enum_form_string_array", ">", "$"))
                .andExpect(status().isOk());

        assertThat(controller.invoked).isTrue();
        assertThat(controller.values).containsExactly(">", "$");
    }

    @Test
    void anAbsentOptionalListIsNull() throws Exception {
        mockMvc.perform(get(FakeApi.PATH_TEST_ENUM_PARAMETERS)
                        .contentType(MediaType.APPLICATION_FORM_URLENCODED))
                .andExpect(status().isOk());

        assertThat(controller.invoked).isTrue();
        assertThat(controller.values).isNull();
    }

    // Use the existing form-array endpoint and inherit annotations from the generated API.
    @RestController
    static class FormArrayController implements FakeApi {
        private List<String> values;
        private boolean invoked;

        @Override
        public ResponseEntity<Void> testEnumParameters(List<String> enumHeaderStringArray, String enumHeaderString,
                List<String> enumQueryStringArray, String enumQueryString, Integer enumQueryInteger,
                Double enumQueryDouble, List<String> enumFormStringArray, String enumFormString) {
            values = enumFormStringArray;
            invoked = true;
            return ResponseEntity.ok().build();
        }
    }
}
