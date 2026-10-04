// @ts-ignore
import http from 'k6/http';
// @ts-ignore
import { FileData } from 'k6/http';

import { ApiResponse } from './ApiResponse.js';
import { Pet } from './Pet.js';

export class PetApi {

    constructor(private baseUrl: string) {}

    /**
    * @summary Add a new pet to the store
    *
    * @returns { Pet } - 200
    * @returns { undefined } - 405
    */
    public addPet(pet: Pet): {
        code: number,
        headers: {
            [key: string]: string,
        },
        body?: Pet | undefined,
    } {
        const url: string = `${this.baseUrl}/pet`;

        const reqHeaders = {
            'Content-Type': `application/json`
        };

        const { code, headers: resHeaders, body } = http.post(url, pet, { headers: reqHeaders });

        let responseBody = undefined;

        if (body) {
            try {
                responseBody = JSON.parse(body);
            } catch (error) {
                responseBody = body;
            }
        }

        return { code, headers: resHeaders, body: responseBody as Pet | undefined };
    }
    /**
    * @summary Deletes a pet
    *
    * @returns { undefined } - 400
    */
    public deletePet(petId: number, apiKey: string): {
        code: number,
        headers: {
            [key: string]: string,
        },
        body?: undefined,
    } {
        const url: string = `${this.baseUrl}/pet/${petId}`;

        const { code, headers: resHeaders, body } = http.delete(url);

        let responseBody = undefined;

        if (body) {
            try {
                responseBody = JSON.parse(body);
            } catch (error) {
                responseBody = body;
            }
        }

        return { code, headers: resHeaders, body: responseBody as undefined };
    }
    /**
    * @summary Finds Pets by status
    *
    * @returns { Pet[] } - 200
    * @returns { undefined } - 400
    */
    public findPetsByStatus(status: string[]): {
        code: number,
        headers: {
            [key: string]: string,
        },
        body?: Pet[] | undefined,
    } {
        const url: string = `${this.baseUrl}/pet/findByStatus?status=${status}`;

        const { code, headers: resHeaders, body } = http.get(url);

        let responseBody = undefined;

        if (body) {
            try {
                responseBody = JSON.parse(body);
            } catch (error) {
                responseBody = body;
            }
        }

        return { code, headers: resHeaders, body: responseBody as Pet[] | undefined };
    }
    /**
    * @summary Finds Pets by tags
    *
    * @returns { Pet[] } - 200
    * @returns { undefined } - 400
    */
    public findPetsByTags(tags: string[]): {
        code: number,
        headers: {
            [key: string]: string,
        },
        body?: Pet[] | undefined,
    } {
        const url: string = `${this.baseUrl}/pet/findByTags?tags=${tags}`;

        const { code, headers: resHeaders, body } = http.get(url);

        let responseBody = undefined;

        if (body) {
            try {
                responseBody = JSON.parse(body);
            } catch (error) {
                responseBody = body;
            }
        }

        return { code, headers: resHeaders, body: responseBody as Pet[] | undefined };
    }
    /**
    * @summary Find pet by ID
    *
    * @returns { Pet } - 200
    * @returns { undefined } - 400
    * @returns { undefined } - 404
    */
    public getPetById(petId: number): {
        code: number,
        headers: {
            [key: string]: string,
        },
        body?: Pet | undefined,
    } {
        const url: string = `${this.baseUrl}/pet/${petId}`;

        const { code, headers: resHeaders, body } = http.get(url);

        let responseBody = undefined;

        if (body) {
            try {
                responseBody = JSON.parse(body);
            } catch (error) {
                responseBody = body;
            }
        }

        return { code, headers: resHeaders, body: responseBody as Pet | undefined };
    }
    /**
    * @summary Update an existing pet
    *
    * @returns { Pet } - 200
    * @returns { undefined } - 400
    * @returns { undefined } - 404
    * @returns { undefined } - 405
    */
    public updatePet(pet: Pet): {
        code: number,
        headers: {
            [key: string]: string,
        },
        body?: Pet | undefined,
    } {
        const url: string = `${this.baseUrl}/pet`;

        const reqHeaders = {
            'Content-Type': `application/json`
        };

        const { code, headers: resHeaders, body } = http.put(url, pet, { headers: reqHeaders });

        let responseBody = undefined;

        if (body) {
            try {
                responseBody = JSON.parse(body);
            } catch (error) {
                responseBody = body;
            }
        }

        return { code, headers: resHeaders, body: responseBody as Pet | undefined };
    }
    /**
    * @summary Updates a pet in the store with form data
    *
    * @returns { undefined } - 405
    */
    public updatePetWithForm(petId: number, name: string, status: string): {
        code: number,
        headers: {
            [key: string]: string,
        },
        body?: undefined,
    } {
        const url: string = `${this.baseUrl}/pet/${petId}`;

        const form = {
            name,
            status,
        };

        const reqHeaders = {
            'Content-Type': `application/x-www-form-urlencoded`
        };

        const { code, headers: resHeaders, body } = http.post(url, form, { headers: reqHeaders });

        let responseBody = undefined;

        if (body) {
            try {
                responseBody = JSON.parse(body);
            } catch (error) {
                responseBody = body;
            }
        }

        return { code, headers: resHeaders, body: responseBody as undefined };
    }
    /**
    * @summary uploads an image
    *
    * @returns { ApiResponse } - 200
    */
    public uploadFile(petId: number, additionalMetadata: string, file: FileData): {
        code: number,
        headers: {
            [key: string]: string,
        },
        body?: ApiResponse,
    } {
        const url: string = `${this.baseUrl}/pet/${petId}/uploadImage`;

        const form = {
            additionalMetadata,
            file,
        };

        const reqHeaders = {
            'Content-Type': `multipart/form-data`
        };

        const { code, headers: resHeaders, body } = http.post(url, form, { headers: reqHeaders });

        let responseBody = undefined;

        if (body) {
            try {
                responseBody = JSON.parse(body);
            } catch (error) {
                responseBody = body;
            }
        }

        return { code, headers: resHeaders, body: responseBody as ApiResponse };
    }
}
