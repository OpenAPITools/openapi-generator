package org.openapitools.model;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.exc.InvalidTypeIdException;
import jakarta.validation.Validator;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;

@SpringBootTest
class AllOfFamilyRecordsTest {

    @Autowired
    private ObjectMapper objectMapper;

    @Autowired
    private Validator validator;

    @Test
    void theParentIsASealedInterfaceAndTheChildrenAreRecordsOfAllProperties() {
        assertThat(Vehicle.class.isSealed()).isTrue();
        assertThat(Vehicle.class.getPermittedSubclasses()).containsExactlyInAnyOrder(Car.class, Bike.class);
        assertThat(Car.class.isRecord() && Bike.class.isRecord()).isTrue();
        assertThat(Vehicle.class.isAssignableFrom(Car.class)).isTrue();
        assertThat(Car.class.getRecordComponents()).extracting("name").containsExactly("type", "wheels", "fuel", "owner", "previousOwners", "registeredAt", "doors");
        assertThat(Bike.class.getRecordComponents()).extracting("name").containsExactly("type", "wheels", "fuel", "owner", "previousOwners", "registeredAt");
        assertThat(Vehicle.FuelEnum.PETROL).isEqualTo(Car.builder().type("car").build().fuel());
    }

    @Test
    void aChildIsReadThroughTheParentTypeByItsDiscriminator() throws Exception {
        Vehicle vehicle = objectMapper.readValue("{\"type\":\"car\",\"wheels\":4,\"doors\":5}", Vehicle.class);

        assertThat(vehicle).isInstanceOf(Car.class);
        Car car = (Car) vehicle;
        assertThat(car.wheels()).isEqualTo(4);
        assertThat(car.doors()).isEqualTo(5);
        assertThat(car.fuel()).as("the parent's default").isEqualTo(Vehicle.FuelEnum.PETROL);
        assertThat(objectMapper.readValue("{\"type\":\"bike\"}", Vehicle.class)).isInstanceOf(Bike.class);
    }

    @Test
    void aChildIsWrittenThroughTheParentTypeWithTheDiscriminatorOnce() throws Exception {
        Vehicle vehicle = Car.builder().type("car").wheels(4).doors(5).fuel(Vehicle.FuelEnum.DIESEL).build();

        String json = objectMapper.writerFor(Vehicle.class).writeValueAsString(vehicle);
        JsonNode tree = objectMapper.readTree(json);

        assertThat(json.split("\"type\"", -1).length - 1).isEqualTo(1);
        assertThat(tree.get("type").asText()).isEqualTo("car");
        assertThat(tree.get("fuel").asText()).isEqualTo("diesel");
        assertThat(objectMapper.readValue(json, Vehicle.class)).isEqualTo(vehicle);
    }

    @Test
    void theParentCannotBeReadUnderItsOwnNameAnymore() {
        assertThatThrownBy(() -> objectMapper.readValue("{\"type\":\"Vehicle\"}", Vehicle.class))
                .isInstanceOf(InvalidTypeIdException.class);
        assertThatThrownBy(() -> objectMapper.readValue("{\"type\":\"truck\"}", Vehicle.class))
                .isInstanceOf(InvalidTypeIdException.class);
    }

    @Test
    void validationReachesTheInheritedPropertiesThroughTheParentType() throws Exception {
        Vehicle missingType = objectMapper.readValue("{\"type\":\"car\"}", Vehicle.class);

        assertThat(validator.validate(missingType)).isEmpty();
        assertThat(validator.validate(Car.builder().build()))
                .extracting(v -> v.getPropertyPath().toString()).containsExactly("type");
    }

    @Test
    void aFamilyMemberWithNestedModelsIsValidatedThroughTheValidatorAndStillCascades() throws Exception {
        // a cascade marker on the interface accessor as well as on the record component makes Hibernate Validator
        // throw a ConstraintDeclarationException (HV000131) for any member, so only the records carry it
        Vehicle valid = objectMapper.readValue(
                "{\"type\":\"car\",\"owner\":{\"id\":1},\"previousOwners\":[{\"id\":2}],"
                        + "\"registeredAt\":\"2026-01-02T03:04:05Z\"}", Vehicle.class);

        assertThat(validator.validate(valid)).isEmpty();
        assertThat(validator.validate(Bike.builder().type("bike").owner(Owner.builder().id(3L).build()).build())).isEmpty();

        Vehicle invalidOwner = objectMapper.readValue("{\"type\":\"car\",\"owner\":{\"email\":\"a@b.co\"}}", Vehicle.class);
        assertThat(validator.validate(invalidOwner)).extracting(v -> v.getPropertyPath().toString())
                .containsExactly("owner.id");
        Vehicle invalidPrevious = objectMapper.readValue(
                "{\"type\":\"bike\",\"previousOwners\":[{\"id\":1},{\"email\":\"a@b.co\"}]}", Vehicle.class);
        assertThat(validator.validate(invalidPrevious)).extracting(v -> v.getPropertyPath().toString())
                .containsExactly("previousOwners[1].id");
    }

    @Test
    void aChildKeepsTheToStringOfARecord() {
        Vehicle vehicle = Bike.builder().type("bike").build();

        assertThat(vehicle).isInstanceOf(Bike.class);
        assertThat(vehicle.toString()).startsWith("Bike[");
    }
}
