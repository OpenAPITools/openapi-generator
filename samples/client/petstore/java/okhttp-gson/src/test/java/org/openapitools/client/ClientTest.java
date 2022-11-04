package org.openapitools.client;

import static org.junit.jupiter.api.Assertions.*;

import java.util.*;

import okhttp3.OkHttpClient;
import org.junit.jupiter.api.*;
import org.openapitools.client.auth.*;

import java.math.BigDecimal;

import org.openapitools.client.model.*;
import org.junit.Assert;
import org.junit.Test;

public class ClientTest {
    ApiClient apiClient;
    JSON json;

    @BeforeEach
    public void setup() {
        apiClient = new ApiClient();
        json = apiClient.getJSON();
    }

    /**
     * Test the property 'arrayArrayNumber'
     */
    @Test
    public void arrayArrayNumberTest() {
        ArrayOfArrayOfNumberOnly model = new ArrayOfArrayOfNumberOnly();
        BigDecimal b1 = new BigDecimal("12.3");
        BigDecimal b2 = new BigDecimal("5.6");
        List<BigDecimal> arrayArrayNumber = new ArrayList<BigDecimal>();
        arrayArrayNumber.add(b1);
        arrayArrayNumber.add(b2);
        model.setArrayArrayNumber(new ArrayList<List<BigDecimal>>());
        model.getArrayArrayNumber().add(arrayArrayNumber);

        // create another instance for comparison
        BigDecimal b3 = new BigDecimal("12.3");
        BigDecimal b4 = new BigDecimal("5.6");
        ArrayOfArrayOfNumberOnly model2 = new ArrayOfArrayOfNumberOnly();
        List<BigDecimal> arrayArrayNumber2 = new ArrayList<BigDecimal>();
        arrayArrayNumber2.add(b1);
        arrayArrayNumber2.add(b2);
        model2.setArrayArrayNumber(new ArrayList<List<BigDecimal>>());
        model2.getArrayArrayNumber().add(arrayArrayNumber2);

        Assert.assertTrue(model2.equals(model));
    }

    /**
     * Model tests for Pet
     */
    @Test
    public void testPet() {
        Pet model = new Pet();
        // test Pet
        model.setId(1029L);
        model.setName("Dog");

        Pet model2 = new Pet();
        model2.setId(1029L);
        model2.setName("Dog");

        Assert.assertTrue(model.equals(model2));
    }
}
