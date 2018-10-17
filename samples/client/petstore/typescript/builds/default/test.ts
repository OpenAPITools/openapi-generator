import {PetApi} from './api';
import {Configuration } from './configuration';

const config = new Configuration();
const api = new PetApi(config);

api.getPetById(3).then((pet) => {
	console.log(pet)
}).catch((err) => {

console.log(err);
});
