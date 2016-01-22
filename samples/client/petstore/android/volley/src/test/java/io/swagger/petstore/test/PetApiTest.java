package es.shyri.swagger.android.volley.petstore.full;

import com.android.volley.ExecutorDelivery;
import com.android.volley.Network;
import com.android.volley.Response;
import com.android.volley.VolleyError;
import com.android.volley.toolbox.BasicNetwork;
import com.android.volley.toolbox.HttpStack;
import com.android.volley.toolbox.HurlStack;
import com.android.volley.toolbox.NoCache;

import net.jodah.concurrentunit.Waiter;

import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.robolectric.RobolectricTestRunner;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;
import java.util.Arrays;
import java.util.List;
import java.util.concurrent.Executors;

import io.swagger.client.ApiException;
import io.swagger.client.ApiInvoker;
import io.swagger.client.api.PetApi;
import io.swagger.client.model.Category;
import io.swagger.client.model.Pet;

import static com.ibm.icu.impl.Assert.fail;
import static junit.framework.Assert.assertTrue;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;


@RunWith(RobolectricTestRunner.class)
public class PetApiTest {
    PetApi api = null;

    @Before
    public void setup() {
        HttpStack stack = new HurlStack();
        Network network = new BasicNetwork(stack);
        ApiInvoker.initializeInstance(new NoCache(), network, 4, new ExecutorDelivery(Executors.newSingleThreadExecutor()), 30);
        api = new PetApi();
    }

    @Test
    public void testCreateAndGetPet() throws Exception {
        final Waiter waiter = new Waiter();
        final Pet pet = createRandomPet();
        api.addPet(pet, new Response.Listener<String>() {
            @Override
            public void onResponse(String response) {
                waiter.resume();
            }
        }, createErrorListener(waiter));

        waiter.await();

        api.getPetById(pet.getId(), new Response.Listener<Pet>() {
            @Override
            public void onResponse(Pet response) {
                Pet fetched = response;
                waiter.assertNotNull(fetched);
                waiter.assertEquals(pet.getId(), fetched.getId());
                waiter.assertNotNull(fetched.getCategory());
                waiter.assertEquals(fetched.getCategory().getName(), pet.getCategory().getName());
                waiter.resume();
            }
        }, createErrorListener(waiter));

        waiter.await();
    }

    @Test
    public void testUpdatePet() throws Exception {
        final Waiter waiter = new Waiter();

        final Pet pet = createRandomPet();
        pet.setName("programmer");

        api.updatePet(pet, new Response.Listener<String>() {
            @Override
            public void onResponse(String response) {
                waiter.resume();
            }
        }, createErrorListener(waiter));

        waiter.await();

        api.getPetById(pet.getId(), new Response.Listener<Pet>() {
            @Override
            public void onResponse(Pet fetched) {
                waiter.assertNotNull(fetched);
                waiter.assertEquals(pet.getId(), fetched.getId());
                waiter.assertNotNull(fetched.getCategory());
                waiter.assertEquals(fetched.getCategory().getName(), pet.getCategory().getName());
                waiter.resume();
            }
        }, createErrorListener(waiter));

        waiter.await();
    }

    @Test
    public void testFindPetsByStatus() throws Exception {
        final Waiter waiter = new Waiter();
        final Pet pet = createRandomPet();
        pet.setName("programmer");
        pet.setStatus(Pet.StatusEnum.available);

        api.updatePet(pet, new Response.Listener<String>() {
            @Override
            public void onResponse(String response) {
                waiter.resume();
            }
        }, createErrorListener(waiter));

        waiter.await();

        api.findPetsByStatus(Arrays.asList(new String[]{"available"}), new Response.Listener<List<Pet>>() {
            @Override
            public void onResponse(List<Pet> pets) {
                waiter.assertNotNull(pets);

                boolean found = false;
                for (Pet fetched : pets) {
                    if (fetched.getId().equals(pet.getId())) {
                        found = true;
                        break;
                    }
                }

                waiter.assertTrue(found);
                waiter.resume();
            }
        }, createErrorListener(waiter));

        waiter.await();
    }

    @Test
    public void testUpdatePetWithForm() throws Exception {
        final Waiter waiter = new Waiter();
        final Pet pet = createRandomPet();
        pet.setName("frank");

        api.addPet(pet, new Response.Listener<String>() {
            @Override
            public void onResponse(String response) {
                waiter.resume();
            }
        }, createErrorListener(waiter));

        waiter.await();

        final Pet[] fetched = new Pet[1];

        api.getPetById(pet.getId(), new Response.Listener<Pet>() {
            @Override
            public void onResponse(Pet petResponse) {
                fetched[0] = petResponse;
                waiter.assertEquals("frank", fetched[0].getName());
                waiter.resume();
            }
        }, createErrorListener(waiter));

        waiter.await();

        api.updatePetWithForm(String.valueOf(fetched[0].getId()), "furt", null, new Response.Listener<String>() {
            @Override
            public void onResponse(String response) {
                waiter.resume();
            }
        }, createErrorListener(waiter));

        waiter.await();

        api.getPetById(fetched[0].getId(), new Response.Listener<Pet>() {
            @Override
            public void onResponse(Pet updated) {
                waiter.assertEquals("furt", updated.getName());
                waiter.resume();
            }
        }, createErrorListener(waiter));

        waiter.await();

    }

    @Test
    public void testDeletePet() throws Exception {
        final Waiter waiter = new Waiter();

        Pet pet = createRandomPet();
        api.addPet(pet, new Response.Listener<String>() {
            @Override
            public void onResponse(String response) {
                waiter.resume();
            }
        }, createErrorListener(waiter));

        waiter.await();

        final Pet[] fetched = new Pet[1];

        api.getPetById(pet.getId(), new Response.Listener<Pet>() {
            @Override
            public void onResponse(Pet response) {
                fetched[0] = response;
                waiter.resume();
            }
        }, createErrorListener(waiter));

        waiter.await();

        api.deletePet(fetched[0].getId(), "special-key", new Response.Listener<String>() {
            @Override
            public void onResponse(String response) {
                waiter.resume();
            }
        }, createErrorListener(waiter));

        waiter.await();


        api.getPetById(fetched[0].getId(), new Response.Listener<Pet>() {
            @Override
            public void onResponse(Pet response) {
                waiter.fail("expected an error");
                waiter.resume();
            }
        }, new Response.ErrorListener() {
            @Override
            public void onErrorResponse(VolleyError error) {
                waiter.assertEquals(404, error.networkResponse.statusCode);
                waiter.resume();
            }
        });
        waiter.await();
    }



    @Test
    public void testUploadFile() throws Exception {
        final Waiter waiter = new Waiter();

        Pet pet = createRandomPet();
        api.addPet(pet, new Response.Listener<String>() {
            @Override
            public void onResponse(String response) {
                waiter.resume();
            }
        }, createErrorListener(waiter));

        waiter.await();

        File file = new File("hello.txt");
        BufferedWriter writer = new BufferedWriter(new FileWriter(file));
        writer.write("Hello world!");
        writer.close();

        api.uploadFile(pet.getId(), "a test file", new File(file.getAbsolutePath()), new Response.Listener<String>() {
            @Override
            public void onResponse(String response) {
                waiter.resume();
            }
        }, createErrorListener(waiter));

        waiter.await();
    }

    @Test
    public void testCreateAndGetPetSync() throws Exception {
        Pet pet = createRandomPet();
        api.addPet(pet);

        Pet fetched = api.getPetById(pet.getId());
        assertNotNull(fetched);
        assertEquals(pet.getId(), fetched.getId());
        assertNotNull(fetched.getCategory());
        assertEquals(fetched.getCategory().getName(), pet.getCategory().getName());
    }

    @Test
    public void testUpdatePetSync() throws Exception {
        Pet pet = createRandomPet();
        pet.setName("programmer");

        api.updatePet(pet);

        Pet fetched = api.getPetById(pet.getId());
        assertNotNull(fetched);
        assertEquals(pet.getId(), fetched.getId());
        assertNotNull(fetched.getCategory());
        assertEquals(fetched.getCategory().getName(), pet.getCategory().getName());
    }

    @Test
    public void testFindPetsByStatusSync() throws Exception {
        Pet pet = createRandomPet();
        pet.setName("programmer");
        pet.setStatus(Pet.StatusEnum.available);

        api.updatePet(pet);

        List<Pet> pets = api.findPetsByStatus(Arrays.asList(new String[]{"available"}));
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

    @Test
    public void testUpdatePetWithFormSync() throws Exception {
        Pet pet = createRandomPet();
        pet.setName("frank");
        api.addPet(pet);

        Pet fetched = api.getPetById(pet.getId());
        assertEquals("frank", fetched.getName());

        api.updatePetWithForm(String.valueOf(fetched.getId()), "furt", null);
        Pet updated = api.getPetById(fetched.getId());
        assertEquals("furt", updated.getName());
    }

    @Test
    public void testDeletePetSync() throws Exception {
        Pet pet = createRandomPet();
        api.addPet(pet);

        Pet fetched = api.getPetById(pet.getId());
        api.deletePet(fetched.getId(), null);

        try {
            fetched = api.getPetById(fetched.getId());
            fail("expected an error");
        } catch (ApiException e) {
            assertEquals(404, e.getCode());
        }
    }

    @Test
    public void testUploadFileSync() throws Exception {
        Pet pet = createRandomPet();
        api.addPet(pet);

        File file = new File("hello.txt");
        BufferedWriter writer = new BufferedWriter(new FileWriter(file));
        writer.write("Hello world!");
        writer.close();

        api.uploadFile(pet.getId(), "a test file", new File(file.getAbsolutePath()));
    }

    private Pet createRandomPet() {
        Pet pet = new Pet();
        pet.setId(System.currentTimeMillis());
        pet.setName("gorilla");

        Category category = new Category();
        category.setName("really-happy");

        pet.setCategory(category);
        pet.setStatus(Pet.StatusEnum.available);
        List<String> photos = Arrays.asList(new String[]{"http://foo.bar.com/1", "http://foo.bar.com/2"});
        pet.setPhotoUrls(photos);

        return pet;
    }

    private Response.ErrorListener createErrorListener(final Waiter waiter) {
        return new Response.ErrorListener() {
            @Override
            public void onErrorResponse(VolleyError error) {
                error.printStackTrace();
                waiter.fail(error.getMessage());
                waiter.resume();
            }
        };
    }
}