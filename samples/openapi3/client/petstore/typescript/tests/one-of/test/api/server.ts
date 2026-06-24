import nock from 'nock';

export const BASE_URL = 'http://localhost:1234';

// Prevent any unmocked requests from making actual HTTP requests.
nock.disableNetConnect();

const mockPath = (path: string, value: any) => {
    const body = JSON.stringify(value);
    const headers = { 'content-type': 'application/json' };
    nock(BASE_URL).get(path).reply(200, body, headers).persist();
}

mockPath(
    '/test',
    {
      "petType": "Cat",
      "name": "Misty"
    }
)

mockPath(
    '/test-discriminator',
    {
      "petType": "dog",
      "bark": "soft"
    }
)



