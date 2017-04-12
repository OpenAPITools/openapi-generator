package io.swagger.model;

import static org.hamcrest.CoreMatchers.*;
import static org.junit.Assert.*;

import java.util.Arrays;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Set;

import org.junit.Before;
import org.junit.Test;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;

/**
 * Tests JSON representation of
 */
public class CapitalizationTest {

    private static final String SMALL_CAMEL = "smallCamel";
    private static final String SMALL_SNAKE = "small_Snake";
    private static final String CAPITAL_CAMEL = "CapitalCamel";
    private static final String CAPITAL_SNAKE = "Capital_Snake";

    private static final String SCA_ETH_FLOW_POINTS = "SCA_ETH_Flow_Points";
    private static final String ATT_NAME = "ATT_NAME";

    private static final ObjectMapper MAPPER = new ObjectMapper();

    private static final Set<String> EXPECTED
        = new HashSet<>(Arrays.asList(SMALL_CAMEL,
                                      SMALL_SNAKE,
                                      CAPITAL_CAMEL,
                                      CAPITAL_SNAKE,
                                      SCA_ETH_FLOW_POINTS,
                                      ATT_NAME));

    private Capitalization sut;


    @Before
    public void setUp() {
        sut = new Capitalization();
        sut.smallCamel(SMALL_CAMEL);
        sut.smallSnake(SMALL_SNAKE);
        sut.capitalCamel(CAPITAL_CAMEL);
        sut.capitalSnake(CAPITAL_SNAKE);
        sut.setScAETHFlowPoints(SCA_ETH_FLOW_POINTS);
        sut.setATTNAME(ATT_NAME);
    }

    @Test
    public void test() throws JsonProcessingException {

         JsonNode json = MAPPER.valueToTree(sut);

         Set<String> fields = new HashSet<>();
         Iterator<String> it = json.fieldNames();
         while (it.hasNext()) {
             fields.add(it.next());
         }

         assertThat(fields, is(equalTo(EXPECTED)));
    }

}
