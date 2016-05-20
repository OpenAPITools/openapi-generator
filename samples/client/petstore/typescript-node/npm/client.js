"use strict";
var api = require('./api');
var fs = require('fs');
var petApi = new api.PetApi();
petApi.setApiKey(api.PetApiApiKeys.api_key, 'special-key');
var tag1 = new api.Tag();
tag1.id = 18291;
tag1.name = 'TS tag 1';
var pet = new api.Pet();
pet.name = 'TypeScriptDoggie';
pet.id = 18291;
pet.photoUrls = ["http://url1", "http://url2"];
pet.tags = [tag1];
var petId;
var exitCode = 0;
petApi.addPet(pet)
    .then(function (res) {
    var newPet = res.body;
    petId = newPet.id;
    console.log("Created pet with ID " + petId);
    newPet.status = api.Pet.StatusEnum.StatusEnum_available;
    return petApi.updatePet(newPet);
})
    .then(function (res) {
    console.log('Updated pet using POST body');
    return petApi.updatePetWithForm(petId, undefined, "pending");
})
    .then(function (res) {
    console.log('Updated pet using POST form');
    return petApi.uploadFile(petId, undefined, fs.createReadStream('sample.png'));
})
    .then(function (res) {
    console.log('Uploaded image');
    return petApi.getPetById(petId);
})
    .then(function (res) {
    console.log('Got pet by ID: ' + JSON.stringify(res.body));
    if (res.body.status != api.Pet.StatusEnum.StatusEnum_pending) {
        throw new Error("Unexpected pet status");
    }
})
    .catch(function (err) {
    console.error(err);
    exitCode = 1;
})
    .finally(function () {
    return petApi.deletePet(petId);
})
    .then(function (res) {
    console.log('Deleted pet');
    process.exit(exitCode);
});
//# sourceMappingURL=client.js.map