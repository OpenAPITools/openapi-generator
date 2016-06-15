if (typeof module === 'object' && module.exports) {
  var expect = require('expect.js');
  var SwaggerPetstore = require('../../src/index');
}

// When testing in the Node.js environment, ApiClient will need access to a Promise constructor.
if (typeof Promise == 'undefined')
  Promise = require('promise');

var api;

beforeEach(function() {
  api = new SwaggerPetstore.PetApi();
});

var getProperty = function(object, getter, property) {
  // Use getter method if present; otherwise, get the property directly.
  if (typeof object[getter] === 'function')
    return object[getter]();
  else
    return object[property];
}

var setProperty = function(object, setter, property, value) {
  // Use setter method if present; otherwise, set the property directly.
  if (typeof object[setter] === 'function')
    object[setter](value);
  else
    object[property] = value;
}

var createRandomPet = function() {
  var id = new Date().getTime();
  var pet = new SwaggerPetstore.Pet();
  setProperty(pet, "setId", "id", id);
  setProperty(pet, "setName", "name", "gorilla" + id);

  var category = new SwaggerPetstore.Category();
  setProperty(category, "setName", "name", "really-happy");
  setProperty(pet, "setCategory", "category", category);

  setProperty(pet, "setStatus", "status", "available");
  var photos = ["http://foo.bar.com/1", "http://foo.bar.com/2"];
  setProperty(pet, "setPhotoUrls", "photoUrls", photos);

  return pet;
};

describe('PetApi', function() {
  it('should create and get pet', function(done) {
    var pet = createRandomPet();
    api.addPet(pet)
        .then(function() {
            return api.getPetById(pet.id)
        })
        .then(function(fetched) {
            expect(fetched).to.be.ok();
            expect(fetched.id).to.be(pet.id);
            expect(getProperty(fetched, "getPhotoUrls", "photoUrls"))
              .to.eql(getProperty(pet, "getPhotoUrls", "photoUrls"));
            expect(getProperty(fetched, "getCategory", "category")).to.be.ok();
            expect(getProperty(getProperty(fetched, "getCategory", "category"), "getName", "name"))
              .to.be(getProperty(getProperty(pet, "getCategory", "category"), "getName", "name"));

            api.deletePet(pet.id);
            done();
        });
  });
});

/*
    @Test
    public void testUpdatePet() throws Exception {
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
    public void testFindPetsByStatus() throws Exception {
        Pet pet = createRandomPet();
        pet.setName("programmer");
        pet.setStatus(Pet.StatusEnum.AVAILABLE);

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
    public void testFindPetsByTags() throws Exception {
        Pet pet = createRandomPet();
        pet.setName("monster");
        pet.setStatus(Pet.StatusEnum.AVAILABLE);

        List<Tag> tags = new ArrayList<Tag>();
        Tag tag1 = new Tag();
        tag1.setName("friendly");
        tags.add(tag1);
        pet.setTags(tags);

        api.updatePet(pet);

        List<Pet> pets = api.findPetsByTags(Arrays.asList(new String[]{"friendly"}));
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
    public void testUpdatePetWithForm() throws Exception {
        Pet pet = createRandomPet();
        pet.setName("frank");
        api.addPet(pet);

        Pet fetched = api.getPetById(pet.getId());

        api.updatePetWithForm(String.valueOf(fetched.getId()), "furt", null);
        Pet updated = api.getPetById(fetched.getId());

        assertEquals(updated.getName(), "furt");
    }

    @Test
    public void testDeletePet() throws Exception {
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
    public void testUploadFile() throws Exception {
        Pet pet = createRandomPet();
        api.addPet(pet);

        File file = new File("hello.txt");
        BufferedWriter writer = new BufferedWriter(new FileWriter(file));
        writer.write("Hello world!");
        writer.close();

        api.uploadFile(pet.getId(), "a test file", new File(file.getAbsolutePath()));
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
}
*/
