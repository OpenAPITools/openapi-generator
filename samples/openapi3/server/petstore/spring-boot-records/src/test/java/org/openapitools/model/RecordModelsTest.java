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
import static org.assertj.core.api.Assertions.assertThatThrownBy;

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
        assertThat(Filter.class.isRecord()).as("bound from request parameters").isTrue();
        assertThat(Range.class.isRecord()).as("nested in the query object").isTrue();
        assertThat(Vehicle.class.isRecord()).isFalse();
        assertThat(Car.class.isRecord()).isFalse();
        assertThat(Counters.class.isRecord()).isFalse();
        assertThat(Circle.class.isRecord()).as("member of a oneOf interface").isTrue();
        assertThat(Square.class.isRecord()).isTrue();
        assertThat(Cat.class.isRecord()).isTrue();
        assertThat(Dog.class.isRecord()).isTrue();
        assertThat(Zoo.class.isRecord()).isTrue();
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

    private static int occurrences(String text, String part) {
        return text.split(java.util.regex.Pattern.quote(part), -1).length - 1;
    }

    @Test
    void theOneOfInterfacesAreSealedAndTheirMembersAreRecords() {
        assertThat(Animal.class.isSealed()).isTrue();
        assertThat(Animal.class.getPermittedSubclasses()).containsExactlyInAnyOrder(Cat.class, Dog.class);
        assertThat(Shape.class.isSealed()).isTrue();
        assertThat(Shape.class.getPermittedSubclasses()).containsExactlyInAnyOrder(Circle.class, Square.class);
        assertThat(Animal.class.isAssignableFrom(Cat.class)).isTrue();
        assertThat(Shape.class.isAssignableFrom(Circle.class)).isTrue();
        assertThat(Cat.class.isRecord() && Dog.class.isRecord() && Circle.class.isRecord() && Square.class.isRecord()).isTrue();
    }

    @Test
    void theDiscriminatorAccessorReturnsTheComponent() throws Exception {
        Animal cat = Cat.builder().animalType("cat").name("Tom").build();

        assertThat(cat.getAnimalType()).isEqualTo("cat");
        assertThat(((Cat) cat).getAnimalType()).isEqualTo(((Cat) cat).animalType());
        assertThat(Animal.class.getMethod("getAnimalType").getReturnType()).isEqualTo(String.class);
        // the accessor carries the JSON name like the class's getter, and nothing else
        for (Class<?> member : List.of(Cat.class, Dog.class)) {
            assertThat(member.getMethod("getAnimalType").getAnnotations()).hasSize(1);
            assertThat(member.getMethod("getAnimalType").getAnnotation(com.fasterxml.jackson.annotation.JsonProperty.class).value())
                    .isEqualTo("animalType");
        }
        // no accessor for an interface without a discriminator
        assertThat(java.util.Arrays.stream(Circle.class.getDeclaredMethods()).map(java.lang.reflect.Method::getName))
                .doesNotContain("getShapeType");
    }

    @Test
    void discriminatorFirstLastAndInTheMiddle() throws Exception {
        Animal first = objectMapper.readValue("{\"animalType\":\"cat\",\"name\":\"Tom\"}", Animal.class);
        Animal middle = objectMapper.readValue("{\"name\":\"Tom\",\"animalType\":\"cat\",\"lives\":3}", Animal.class);
        Animal last = objectMapper.readValue("{\"name\":\"Rex\",\"barks\":true,\"animalType\":\"dog\"}", Animal.class);

        assertThat(first).isEqualTo(new Cat("cat", "Tom", null));
        assertThat(((Cat) first).lives()).isEqualTo(9);
        assertThat(((Cat) middle).lives()).isEqualTo(3);
        assertThat(((Cat) middle).animalType()).isEqualTo("cat");
        assertThat(last).isInstanceOf(Dog.class);
        assertThat(((Dog) last).name()).isEqualTo("Rex");
        assertThat(((Dog) last).barks()).isTrue();
        assertThat(((Dog) last).animalType()).isEqualTo("dog");
    }

    @Test
    void discriminatorAbsentUnknownOrNullIsRefused() {
        assertThatThrownBy(() -> objectMapper.readValue("{\"name\":\"Tom\"}", Animal.class))
                .hasMessageContaining("animalType");
        assertThatThrownBy(() -> objectMapper.readValue("{\"animalType\":\"bird\",\"name\":\"Tom\"}", Animal.class))
                .hasMessageContaining("bird");
        assertThatThrownBy(() -> objectMapper.readValue("{\"animalType\":null,\"name\":\"Tom\"}", Animal.class))
                .isInstanceOf(Exception.class);
    }

    @Test
    void jsonNullIsNullThroughTheInterfaceType() throws Exception {
        assertThat(objectMapper.readValue("null", Animal.class)).isNull();
    }

    @Test
    void writingThroughTheInterfaceWritesTheDiscriminatorOnce() throws Exception {
        Animal cat = new Cat("cat", "Tom", null);
        Animal dog = new Dog("Rex", true, "dog");

        String viaInterface = objectMapper.writerFor(Animal.class).writeValueAsString(cat);
        String dogViaInterface = objectMapper.writerFor(Animal.class).writeValueAsString(dog);

        assertThat(occurrences(viaInterface, "\"animalType\"")).isEqualTo(1);
        assertThat(objectMapper.readTree(viaInterface).get("animalType").asText()).isEqualTo("cat");
        assertThat(objectMapper.readTree(viaInterface).get("lives").asInt()).isEqualTo(9);
        assertThat(occurrences(dogViaInterface, "\"animalType\"")).isEqualTo(1);
        assertThat(objectMapper.readValue(viaInterface, Animal.class)).isEqualTo(cat);
        assertThat(objectMapper.readValue(dogViaInterface, Animal.class)).isEqualTo(dog);
    }

    @Test
    void aListOfTheInterfaceTypeRoundTrips() throws Exception {
        String json = "{\"keeper\":\"Ann\",\"animals\":[{\"animalType\":\"cat\",\"name\":\"Tom\",\"lives\":2},"
                + "{\"animalType\":\"dog\",\"name\":\"Rex\"}],\"mascot\":{\"animalType\":\"dog\",\"name\":\"Max\"}}";

        Zoo zoo = objectMapper.readValue(json, Zoo.class);
        String written = objectMapper.writeValueAsString(zoo);
        JsonNode tree = objectMapper.readTree(written);

        assertThat(zoo.animals()).hasSize(2);
        assertThat(zoo.animals().get(0)).isInstanceOf(Cat.class);
        assertThat(zoo.animals().get(1)).isInstanceOf(Dog.class);
        assertThat(zoo.mascot()).isEqualTo(new Dog("Max", null, "dog"));
        assertThat(tree.get("animals").get(0).get("animalType").asText()).isEqualTo("cat");
        assertThat(tree.get("animals").get(1).get("animalType").asText()).isEqualTo("dog");
        assertThat(tree.get("mascot").get("animalType").asText()).isEqualTo("dog");
        assertThat(occurrences(written, "\"animalType\"")).isEqualTo(3);
        assertThat(objectMapper.readValue(written, Zoo.class)).isEqualTo(zoo).hasSameHashCodeAs(zoo);
    }

    @Test
    void validationReachesTheMembersThroughTheInterfaceType() throws Exception {
        Zoo zoo = objectMapper.readValue("{\"animals\":[{\"animalType\":\"cat\",\"name\":\"Tom\"},"
                + "{\"animalType\":\"dog\",\"name\":\"R\"}],\"mascot\":{\"animalType\":\"cat\",\"name\":\"T\"}}", Zoo.class);

        Set<String> paths = validator.validate(zoo).stream().map(v -> v.getPropertyPath().toString()).collect(Collectors.toSet());

        assertThat(paths).containsExactlyInAnyOrder("animals[1].name", "mascot.name");
        assertThat(validator.validate(objectMapper.readValue("{\"animals\":[{\"animalType\":\"cat\",\"name\":\"Tom\"}]}", Zoo.class)))
                .isEmpty();
    }

    @Test
    void membersOfAnInterfaceWithoutADiscriminatorAreWrittenAsTheyAre() throws Exception {
        Shape circle = new Circle(1.5);
        String json = objectMapper.writeValueAsString(circle);

        assertThat(objectMapper.readTree(json).get("radius").asDouble()).isEqualTo(1.5);
        assertThat(circle).isInstanceOf(Shape.class);
    }
}
