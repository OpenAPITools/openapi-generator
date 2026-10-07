package org.openapitools.model;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import jakarta.validation.ConstraintViolation;
import jakarta.validation.Validator;
import org.junit.jupiter.api.Test;
import org.openapitools.jackson.nullable.JsonNullable;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;

import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;

import static org.assertj.core.api.Assertions.assertThat;

@SpringBootTest
class RecordModelsTest {

    @Autowired
    private ObjectMapper objectMapper;

    @Autowired
    private Validator validator;

    private Pet readPet(String json) throws Exception {
        return objectMapper.readValue(json, Pet.class);
    }

    private static Set<String> invalidPaths(Set<ConstraintViolation<Pet>> violations) {
        return violations.stream().map(v -> v.getPropertyPath().toString()).collect(Collectors.toSet());
    }

    @Test
    void plainModelsAreRecordsAndTheOthersStayClasses() {
        assertThat(Pet.class.isRecord()).isTrue();
        assertThat(Owner.class.isRecord()).isTrue();
        assertThat(Account.class.isRecord()).isTrue();
        assertThat(Filter.class.isRecord()).as("bound from request parameters").isFalse();
        assertThat(Vehicle.class.isRecord()).isFalse();
        assertThat(Car.class.isRecord()).isFalse();
        assertThat(Counters.class.isRecord()).isFalse();
        assertThat(Circle.class.isRecord()).isFalse();
        assertThat(Square.class.isRecord()).isFalse();
    }

    @Test
    void absentNullableIsUndefined() throws Exception {
        Pet pet = readPet("{\"name\":\"rex\"}");

        assertThat(pet.nickname().isPresent()).isFalse();
        assertThat(objectMapper.writeValueAsString(pet)).doesNotContain("nickname");
    }

    @Test
    void explicitNullNullableIsPresentAndNull() throws Exception {
        Pet pet = readPet("{\"name\":\"rex\",\"nickname\":null}");

        assertThat(pet.nickname().isPresent()).isTrue();
        assertThat(pet.nickname().get()).isNull();
        assertThat(objectMapper.writeValueAsString(pet)).contains("\"nickname\":null");
    }

    @Test
    void nullableWithValue() throws Exception {
        Pet pet = readPet("{\"name\":\"rex\",\"nickname\":\"rexy\"}");

        assertThat(pet.nickname()).isEqualTo(JsonNullable.of("rexy"));
        assertThat(objectMapper.writeValueAsString(pet)).contains("\"nickname\":\"rexy\"");
    }

    @Test
    void defaultsApplyToAbsentProperties() throws Exception {
        Pet pet = readPet("{\"name\":\"rex\"}");

        assertThat(pet.age()).isEqualTo(1);
        assertThat(pet.status()).isEqualTo(Pet.StatusEnum.AVAILABLE);
        assertThat(pet.labels()).isEmpty();
        assertThat(pet.colors()).containsExactly("red", "blue");
        assertThat(pet.tag()).isNull();
        assertThat(pet.owner()).isNull();
    }

    @Test
    void defaultsApplyToExplicitNull() throws Exception {
        // A record cannot tell null from absent in its constructor; the class keeps the null.
        Pet pet = readPet("{\"name\":\"rex\",\"age\":null,\"status\":null,\"labels\":null,\"colors\":null,\"tag\":null}");

        assertThat(pet.age()).isEqualTo(1);
        assertThat(pet.status()).isEqualTo(Pet.StatusEnum.AVAILABLE);
        assertThat(pet.labels()).isEmpty();
        assertThat(pet.colors()).containsExactly("red", "blue");
        assertThat(pet.tag()).isNull();
    }

    @Test
    void uniqueItemsKeepTheOrderOfTheDocument() throws Exception {
        Pet pet = readPet("{\"name\":\"rex\",\"aliases\":[\"b\",\"a\",\"c\"]}");

        assertThat(pet.aliases()).containsExactly("b", "a", "c");
        assertThat(readPet("{\"name\":\"rex\"}").aliases()).isEmpty();
    }

    @Test
    void canonicalConstructorAppliesDefaultsToNulls() {
        Pet pet = new Pet("rex", null, null, null, null, null, null, null, null, null, null);

        assertThat(pet.name()).isEqualTo("rex");
        assertThat(pet.nickname().isPresent()).isFalse();
        assertThat(pet.age()).isEqualTo(1);
        assertThat(pet.colors()).containsExactly("red", "blue");
        assertThat(Pet.class.getConstructors()).hasSize(1);
    }

    @Test
    void topLevelValidationFails() throws Exception {
        assertThat(invalidPaths(validator.validate(readPet("{\"tag\":\"t\"}")))).containsExactly("name");
        assertThat(invalidPaths(validator.validate(readPet("{\"name\":\"a\"}")))).containsExactly("name");
        assertThat(validator.validate(readPet("{\"name\":\"rex\"}"))).isEmpty();
    }

    @Test
    void nestedValidationFails() throws Exception {
        Pet missingId = readPet("{\"name\":\"rex\",\"owner\":{\"email\":\"a@b.co\"}}");
        Pet badEmail = readPet("{\"name\":\"rex\",\"owner\":{\"id\":1,\"email\":\"nope\"}}");

        assertThat(invalidPaths(validator.validate(missingId))).containsExactly("owner.id");
        assertThat(invalidPaths(validator.validate(badEmail))).containsExactly("owner.email");
    }

    @Test
    void unknownPropertiesAreIgnored() throws Exception {
        Pet pet = readPet("{\"name\":\"rex\",\"bogus\":1,\"owner\":{\"id\":1,\"bogus\":2}}");

        assertThat(pet.owner().id()).isEqualTo(1L);
    }

    @Test
    void roundTrip() throws Exception {
        String json = "{\"name\":\"rex\",\"tag\":\"t\",\"nickname\":\"rexy\",\"age\":5,\"kind\":\"dog\","
                + "\"status\":\"sold\",\"labels\":[\"a\",\"b\"],\"colors\":[\"green\"],\"aliases\":[\"z\",\"y\"],"
                + "\"owner\":{\"id\":7,\"email\":\"a@b.co\"},\"born\":\"2020-01-02T03:04:05Z\"}";

        Pet pet = readPet(json);
        JsonNode written = objectMapper.readTree(objectMapper.writeValueAsString(pet));

        assertThat(written).isEqualTo(objectMapper.readTree(json));
        assertThat(readPet(objectMapper.writeValueAsString(pet))).isEqualTo(pet).hasSameHashCodeAs(pet);
        assertThat(pet.kind()).isEqualTo(PetKind.DOG);
        assertThat(pet.labels()).isEqualTo(List.of("a", "b"));
    }

    @Test
    void passwordIsNotPrinted() throws Exception {
        Account account = objectMapper.readValue(
                "{\"username\":\"bob\",\"password\":\"s3cret\"}", Account.class);

        assertThat(account.password()).isEqualTo("s3cret");
        assertThat(account.toString()).contains("username=bob").contains("password=*").doesNotContain("s3cret");
        assertThat(readPet("{\"name\":\"rex\"}").toString()).startsWith("Pet[name=rex");
    }

    @Test
    void builderAppliesTheInitialValues() {
        Pet pet = Pet.builder().name("rex").build();

        assertThat(pet).isEqualTo(new Pet("rex", null, null, null, null, null, null, null, null, null, null));
        assertThat(pet.nickname().isPresent()).isFalse();
        assertThat(pet.age()).isEqualTo(1);
        assertThat(pet.status()).isEqualTo(Pet.StatusEnum.AVAILABLE);
        assertThat(pet.labels()).isEmpty();
        assertThat(pet.colors()).containsExactly("red", "blue");
        assertThat(pet.aliases()).isEmpty();
    }

    @Test
    void builderSetsEveryComponent() {
        Owner owner = Owner.builder().id(7L).email("a@b.co").build();
        Pet pet = Pet.builder().name("rex").tag("t").nickname("rexy").age(5).kind(PetKind.DOG)
                .status(Pet.StatusEnum.SOLD).labels(List.of("a")).colors(List.of("green"))
                .aliases(Set.of("z")).owner(owner).born(java.time.OffsetDateTime.parse("2020-01-02T03:04:05Z")).build();

        assertThat(pet).isEqualTo(new Pet("rex", "t", JsonNullable.of("rexy"), 5, PetKind.DOG, Pet.StatusEnum.SOLD,
                List.of("a"), List.of("green"), Set.of("z"), owner, java.time.OffsetDateTime.parse("2020-01-02T03:04:05Z")));
    }

    @Test
    void builderNullableComponentHasBothOverloads() {
        Pet value = Pet.builder().name("rex").nickname("rexy").build();
        Pet explicitNull = Pet.builder().name("rex").nickname(JsonNullable.of((String) null)).build();
        Pet explicitNullThroughValue = Pet.builder().name("rex").nickname((String) null).build();
        Pet undefined = Pet.builder().name("rex").nickname(JsonNullable.<String>undefined()).build();
        Pet neverSet = Pet.builder().name("rex").build();

        assertThat(value.nickname()).isEqualTo(JsonNullable.of("rexy"));
        assertThat(explicitNull.nickname().isPresent()).isTrue();
        assertThat(explicitNull.nickname().get()).isNull();
        assertThat(explicitNullThroughValue.nickname()).isEqualTo(explicitNull.nickname());
        assertThat(undefined.nickname().isPresent()).isFalse();
        assertThat(neverSet.nickname().isPresent()).isFalse();
    }

    @Test
    void builderNullGivesTheInitialValue() {
        // the class's builder keeps a null; a record cannot tell it from "not set"
        Pet pet = Pet.builder().name("rex").age(null).status(null).labels(null).build();

        assertThat(pet.age()).isEqualTo(1);
        assertThat(pet.status()).isEqualTo(Pet.StatusEnum.AVAILABLE);
        assertThat(pet.labels()).isEmpty();
    }

    @Test
    void toBuilderRoundTripGivesAnEqualRecord() throws Exception {
        Pet pet = readPet("{\"name\":\"rex\",\"tag\":\"t\",\"nickname\":null,\"age\":5,\"kind\":\"dog\","
                + "\"status\":\"sold\",\"labels\":[\"a\"],\"colors\":[\"green\"],\"aliases\":[\"z\"],"
                + "\"owner\":{\"id\":7},\"born\":\"2020-01-02T03:04:05Z\"}");

        Pet copy = pet.toBuilder().build();

        assertThat(copy).isEqualTo(pet).hasSameHashCodeAs(pet).isNotSameAs(pet);
        assertThat(copy.nickname().isPresent()).isTrue();
        assertThat(pet.toBuilder().name("max").build()).isEqualTo(
                new Pet("max", pet.tag(), pet.nickname(), pet.age(), pet.kind(), pet.status(), pet.labels(),
                        pet.colors(), pet.aliases(), pet.owner(), pet.born()));
        assertThat(readPet("{\"name\":\"rex\"}").toBuilder().build()).isEqualTo(readPet("{\"name\":\"rex\"}"));
    }

    @Test
    void aBuilderCanBeUsedAgainAfterBuild() {
        Pet.Builder builder = Pet.builder().name("rex").age(3);

        Pet first = builder.build();
        Pet second = builder.build();
        Pet third = builder.name("max").build();

        assertThat(first).isEqualTo(second).isNotSameAs(second);
        assertThat(third.name()).isEqualTo("max");
        assertThat(third.age()).isEqualTo(3);
        assertThat(first.name()).isEqualTo("rex");
    }

    @Test
    void builderOfARecordWithAPasswordDoesNotPrintIt() {
        Account account = Account.builder().username("bob").password("s3cret").build();

        assertThat(account.password()).isEqualTo("s3cret");
        assertThat(account.toString()).contains("username=bob").contains("password=*").doesNotContain("s3cret");
        assertThat(account.toBuilder().build().toString()).doesNotContain("s3cret");
        assertThat(Account.builder().password("s3cret").toString()).doesNotContain("s3cret");
    }

    @Test
    void jacksonIgnoresTheBuilder() throws Exception {
        Pet pet = Pet.builder().name("rex").nickname("rexy").build();
        JsonNode written = objectMapper.readTree(objectMapper.writeValueAsString(pet));

        assertThat(written.size()).isEqualTo(7);
        assertThat(written.has("builder")).isFalse();
        assertThat(written.has("toBuilder")).isFalse();
        assertThat(readPet(objectMapper.writeValueAsString(pet))).isEqualTo(pet);
        assertThat(Pet.class.getConstructors()).hasSize(1);
    }

    @Test
    void modelsThatStayClassesKeepTheirBuilder() {
        Vehicle vehicle = Vehicle.builder().type("car").wheels(4).build();

        assertThat(vehicle.getWheels()).isEqualTo(4);
        assertThat(Car.builder().type("car").doors(5).build().getDoors()).isEqualTo(5);
    }
}
