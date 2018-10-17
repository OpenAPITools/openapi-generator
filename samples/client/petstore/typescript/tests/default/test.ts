import {PetApi, Configuration, Middleware, RequestContext, ResponseContext} from 'ts-petstore-client';

class MiddlewareA implements Middleware {

	public pre(request: RequestContext) {
		console.log(request);
		return Promise.resolve(request);
	}

	public post(response: ResponseContext) {
		console.log(response)
		return Promise.resolve(response);
	}
}

const config = new Configuration({
	middleware: [
		new MiddlewareA()
	]
});
const api = new PetApi(config);

api.getPetById(3).then((pet) => {
	console.log(pet)
}).catch((err) => {
	console.log(err);
});
