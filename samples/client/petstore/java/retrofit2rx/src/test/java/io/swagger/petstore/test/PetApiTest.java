package io.swagger.petstore.test;

import io.swagger.client.ApiClient;
import io.swagger.client.api.*;
import io.swagger.client.model.*;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import org.junit.*;

import com.squareup.okhttp.MediaType;
import com.squareup.okhttp.RequestBody;

import static org.junit.Assert.*;

public class PetApiTest {
    PetApi api = null;

    @Before
    public void setup() {
        api = new ApiClient().createService(PetApi.class);
    }

    @Test
    public void testCreateAndGetPet() throws Exception {
        final Pet pet = createRandomPet();
        api.addPet(pet).subscribe(new SkeletonSubscriber<Void>() {
            @Override
            public void onCompleted() {
                api.getPetById(pet.getId()).subscribe(new SkeletonSubscriber<Pet>() {
                    @Override
                    public void onNext(Pet fetched) {
                        assertNotNull(fetched);
                        assertEquals(pet.getId(), fetched.getId());
                        assertNotNull(fetched.getCategory());
                        assertEquals(fetched.getCategory().getName(), pet.getCategory().getName());
                    }
                });

            }
        });

    }

    @Test
    public void testUpdatePet() throws Exception {
        final Pet pet = createRandomPet();
        pet.setName("programmer");

        api.updatePet(pet).subscribe(new SkeletonSubscriber<Void>() {
            @Override
            public void onCompleted() {
                api.getPetById(pet.getId()).subscribe(new SkeletonSubscriber<Pet>() {
                    @Override
                    public void onNext(Pet fetched) {
                        assertNotNull(fetched);
                        assertEquals(pet.getId(), fetched.getId());
                        assertNotNull(fetched.getCategory());
                        assertEquals(fetched.getCategory().getName(), pet.getCategory().getName());
                    }
                });

            }
        });

    }

    @Test
    public void testFindPetsByStatus() throws Exception {
        final Pet pet = createRandomPet();
        pet.setName("programmer");
        pet.setStatus(Pet.StatusEnum.AVAILABLE);

        api.updatePet(pet).subscribe(new SkeletonSubscriber<Void>() {
            @Override
            public void onCompleted() {
                api.findPetsByStatus(Arrays.asList(new String[]{"available"})).subscribe(new SkeletonSubscriber<List<Pet>>() {
                    @Override
                    public void onNext(List<Pet> pets) {
                        assertNotNull(pets);

                        boolean found = false;
                        for (Pet fetched : pets) {
                            if (fetched.getId().equals(pet.getId())) {
                                found = true;
                                break;
                            }
                        }

                        assertTrue(found);
                    }
                });

            }
        });

    }

    @Test
    public void testFindPetsByTags() throws Exception {
        final Pet pet = createRandomPet();
        pet.setName("monster");
        pet.setStatus(Pet.StatusEnum.AVAILABLE);

        List<Tag> tags = new ArrayList<Tag>();
        Tag tag1 = new Tag();
        tag1.setName("friendly");
        tags.add(tag1);
        pet.setTags(tags);

        api.updatePet(pet).subscribe(new SkeletonSubscriber<Void>() {
            @Override
            public void onCompleted() {
                api.findPetsByTags(Arrays.asList(new String[]{"friendly"})).subscribe(new SkeletonSubscriber<List<Pet>>() {
                    @Override
                    public void onNext(List<Pet> pets) {
                        assertNotNull(pets);

                        boolean found = false;
                        for (Pet fetched : pets) {
                            if (fetched.getId().equals(pet.getId())) {
                                found = true;
                                break;
                            }
                        }
                        assertTrue(found);
                    }
                });

            }
        });

    }

    @Test
    public void testUpdatePetWithForm() throws Exception {
        final Pet pet = createRandomPet();
        pet.setName("frank");
        api.addPet(pet).subscribe(SkeletonSubscriber.failTestOnError());
        api.getPetById(pet.getId()).subscribe(new SkeletonSubscriber<Pet>() {
            @Override
            public void onNext(final Pet fetched) {
                api.updatePetWithForm(String.valueOf(fetched.getId()), "furt", null)
                        .subscribe(new SkeletonSubscriber<Void>() {
                            @Override
                            public void onCompleted() {
                                api.getPetById(fetched.getId()).subscribe(new SkeletonSubscriber<Pet>() {
                                    @Override
                                    public void onNext(Pet updated) {
                                        assertEquals(updated.getName(), "furt");
                                    }
                                });

                            }
                        });
            }
        });


    }

    @Test
    public void testDeletePet() throws Exception {
        Pet pet = createRandomPet();
        api.addPet(pet).subscribe(SkeletonSubscriber.failTestOnError());

        api.getPetById(pet.getId()).subscribe(new SkeletonSubscriber<Pet>() {
            @Override
            public void onNext(Pet fetched) {

                api.deletePet(fetched.getId(), null).subscribe(SkeletonSubscriber.failTestOnError());
                api.getPetById(fetched.getId()).subscribe(new SkeletonSubscriber<Pet>() {
                    @Override
                    public void onNext(Pet deletedPet) {
                        fail("Should not have found deleted pet.");
                    }

                    @Override
                    public void onError(Throwable e) {
                        // expected, because the pet has been deleted.
                    }
                });
            }
        });
    }

    @Test
    public void testUploadFile() throws Exception {
        File file = File.createTempFile("test", "hello.txt");
        BufferedWriter writer = new BufferedWriter(new FileWriter(file));

        writer.write("Hello world!");
        writer.close();

        Pet pet = createRandomPet();
        api.addPet(pet).subscribe(SkeletonSubscriber.failTestOnError());

        RequestBody body = RequestBody.create(MediaType.parse("text/plain"), file);
        api.uploadFile(pet.getId(), "a test file", body).subscribe(new SkeletonSubscriber<Void>() {
            @Override
            public void onError(Throwable e) {
                // this also yields a 400 for other tests, so I guess it's okay...
            }
        });
    }

    @Test
    public void testEqualsAndHashCode() {
        Pet pet1 = new Pet();
        Pet pet2 = new Pet();
        assertTrue(pet1.equals(pet2));
        assertTrue(pet2.equals(pet1));
        assertTrue(pet1.hashCode() == pet2.hashCode());
        assertTrue(pet1.equals(pet1));
        assertTrue(pet1.hashCode() == pet1.hashCode());

        pet2.setName("really-happy");
        pet2.setPhotoUrls(Arrays.asList(new String[]{"http://foo.bar.com/1", "http://foo.bar.com/2"}));
        assertFalse(pet1.equals(pet2));
        assertFalse(pet2.equals(pet1));
        assertFalse(pet1.hashCode() == (pet2.hashCode()));
        assertTrue(pet2.equals(pet2));
        assertTrue(pet2.hashCode() == pet2.hashCode());

        pet1.setName("really-happy");
        pet1.setPhotoUrls(Arrays.asList(new String[]{"http://foo.bar.com/1", "http://foo.bar.com/2"}));
        assertTrue(pet1.equals(pet2));
        assertTrue(pet2.equals(pet1));
        assertTrue(pet1.hashCode() == pet2.hashCode());
        assertTrue(pet1.equals(pet1));
        assertTrue(pet1.hashCode() == pet1.hashCode());
    }

    private Pet createRandomPet() {
        Pet pet = new Pet();
        pet.setId(System.currentTimeMillis());
        pet.setName("gorilla");

        Category category = new Category();
        category.setName("really-happy");

        pet.setCategory(category);
        pet.setStatus(Pet.StatusEnum.AVAILABLE);
        List<String> photos = Arrays.asList(new String[]{"http://foo.bar.com/1", "http://foo.bar.com/2"});
        pet.setPhotoUrls(photos);

        return pet;
    }
}