import api = require('./api');
import fs = require('fs');

var petApi = new api.PetApi('http://petstore.swagger.io');
petApi.apiKey = 'special-key';

var pet = new api.Pet();
pet.name = 'TypeScriptDoggie';

var petId: any;

var exitCode = 0;

// Test various API calls to the petstore
petApi.addPet(pet)
.then((res) => {
	var newPet = <api.Pet>(<any>res.response).body;
	petId = (<any>res.response).body.id;
	console.log(`Created pet with ID ${petId}`);
	newPet.status = api.Pet.StatusEnum.available;
	return petApi.updatePet(newPet);
})
.then((res) => {
	console.log('Updated pet using POST body');
	return petApi.updatePetWithForm(petId, undefined, "pending");
})
.then((res) => {
	console.log('Updated pet using POST form');
	return petApi.uploadFile(petId, undefined, fs.createReadStream('sample.png'));
})
.then((res) => {
	console.log('Uploaded image');
	return petApi.getPetById(petId);
})
.then((res) => {
	console.log('Got pet by ID: ' + JSON.stringify(res.body));
	if (res.body.status != api.Pet.StatusEnum.pending) {
		throw new Error("Unexpected pet status");
	}
})
.catch((err:any) => {
	console.error(err);
	exitCode = 1;
})
.finally(() => {
	return petApi.deletePet(petId);
})
.then((res) => {
	console.log('Deleted pet');
	process.exit(exitCode);
});
