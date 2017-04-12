import api = require('./api');
import fs = require('fs');

var petApi = new api.PetApi();
petApi.setApiKey(api.PetApiApiKeys.api_key, 'special-key');
//petApi.setApiKey(api.PetApiApiKeys.test_api_key_header, 'query-key');

var tag1 = new api.Tag();
tag1.id = 18291;
tag1.name = 'TS tag 1';

var pet = new api.Pet();
pet.name = 'TypeScriptDoggie';
pet.id = 18291;
pet.photoUrls = ["http://url1", "http://url2"];
pet.tags = [tag1];

var petId: any;

var exitCode = 0;

// Test various API calls to the petstore
petApi.addPet(pet)
    .then((res) => {
        var newPet = <api.Pet>res.body;
        petId = newPet.id;
        console.log(`Created pet with ID ${petId}`);
        newPet.status = api.Pet.StatusEnum.Available;
        return petApi.updatePet(newPet);
    })
    .then((res) => {
        console.log('Updated pet using POST body');
        return petApi.updatePetWithForm(petId, undefined, "pending");
    })
    .then((res) => {
        console.log('Updated pet using POST form');
        return petApi.uploadFile(petId, undefined, fs.readFileSync('sample.png'));
    })
    .then((res) => {
        console.log('Uploaded image');
        return petApi.getPetById(petId);
    })
    .then((res) => {
        console.log('Got pet by ID: ' + JSON.stringify(res.body));
        if (res.body.status != api.Pet.StatusEnum.Pending) {
            throw new Error("Unexpected pet status");
        }
    })
    .catch((err: any) => {
        console.error(err);
        exitCode = 1;
    })
    .then(() => {
        return petApi.deletePet(petId);
    })
    .then((res) => {
        console.log('Deleted pet');
        process.exit(exitCode);
    });
