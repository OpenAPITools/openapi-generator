import api = require('./api');

var petApi = new api.PetApi('http://petstore.swagger.io', undefined, undefined);

var pet = new api.Pet();
pet.name = 'TypeScriptDoggie';

var petId: any;

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
	return petApi.getPetById(petId);
})
.then((res) => {
	console.log('Got pet by ID: ' + JSON.stringify(res.body));
	if (res.body.status != api.Pet.StatusEnum.pending) {
		throw new Error("Unexpected pet status");
	}
	return petApi.deletePet(petId);
})
.then((res) => {
	console.log('Deleted pet');
})
.catch((err:any) => {
	console.error(err);
});

//var pets = petApi.findPetsByStatus(['available']);

//pets.then((data:any) => {
//console.log(data);	
//});
