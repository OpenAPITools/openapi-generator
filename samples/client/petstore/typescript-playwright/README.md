## openapi-playwright-tests@1.0.0

This project contains a typed API client and a set of [Playwright](https://playwright.dev/)
API tests generated from an OpenAPI specification by
[OpenAPI Generator](https://openapi-generator.tech).

### Layout

```
apis/                 typed API client classes (one per tag), driven by Playwright's APIRequestContext
models/               request/response models with (de)serializers
runtime.ts            BaseAPI + helpers wrapping Playwright's `request` fixture
spec/apis/*.spec.ts   generated API tests (one describe block per API, one test per operation)
spec/models/*.spec.ts generated model type tests
playwright.config.ts  Playwright configuration
```

### Installing

```
npm install
npx playwright install
```

### Type-checking

```
npm run typecheck
```

### Running the tests

The generated tests issue real HTTP requests through Playwright's `request`
fixture. Point them at a running server with the `BASE_URL` environment
variable (it defaults to the `servers` entry from the OpenAPI document):

```
BASE_URL=http://localhost:8080 npm test
```

Each generated test instantiates an API class with the Playwright `request`
fixture, calls the operation and asserts on the response status code:

```ts
test('addPet', async ({ request }) => {
    const api = new PetApi(request);
    const response = await api.addPetRaw({ pet: {} as unknown as Pet });
    expect(response.raw.status()).toBe(200);
});
```

The example request data is a generated placeholder — replace it with
realistic values and extend the assertions to cover response bodies.
