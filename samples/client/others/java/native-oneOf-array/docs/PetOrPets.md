

# PetOrPets

## oneOf schemas
* [List<Pet>](List<Pet>.md)
* [Pet](Pet.md)

## Example
```java
// Import classes:
import org.openapitools.client.model.PetOrPets;
import org.openapitools.client.model.List<Pet>;
import org.openapitools.client.model.Pet;

public class Example {
    public static void main(String[] args) {
        PetOrPets examplePetOrPets = new PetOrPets();

        // create a new List<Pet>
        List<Pet> exampleList<Pet> = new List<Pet>();
        // set PetOrPets to List<Pet>
        examplePetOrPets.setActualInstance(exampleList<Pet>);
        // to get back the List<Pet> set earlier
        List<Pet> testList<Pet> = (List<Pet>) examplePetOrPets.getActualInstance();

        // create a new Pet
        Pet examplePet = new Pet();
        // set PetOrPets to Pet
        examplePetOrPets.setActualInstance(examplePet);
        // to get back the Pet set earlier
        Pet testPet = (Pet) examplePetOrPets.getActualInstance();
    }
}
```


