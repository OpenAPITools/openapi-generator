// @ts-ignore
import http from 'k6/http';
// @ts-ignore
import { FileData } from 'k6/http';

import { Pet } from './Pet.js';

export class PetApi {

    constructor(private baseUrl: string) {}

    public addPet(pet: Pet): {
        code: number,
        headers: {
            [key: string]: string,
        },
        body?: string,
    } {
        const url: string = `${this.baseUrl}/pet`;

        const reqHeaders = {
            'Content-Type': `application/json`
        };

        const { code, headers: resHeaders, body } = http.post(url, pet, { headers: reqHeaders });

        return { code, headers: resHeaders };
    }
    public deletePet(petId: number, apiKey: string): {
        code: number,
        headers: {
            [key: string]: string,
        },
        body?: string,
    } {
        const url: string = `${this.baseUrl}/pet/${petId}`;

        const { code, headers: resHeaders, body } = http.delete(url);

        return { code, headers: resHeaders };
    }
    public findPetsByStatus(status: any[]): {
        code: number,
        headers: {
            [key: string]: string,
        },
        body?: string,
    } {
        const url: string = `${this.baseUrl}/pet/findByStatus?status=${status}`;

        const { code, headers: resHeaders, body } = http.get(url);

        return { code, headers: resHeaders };
    }
    public findPetsByTags(tags: any[]): {
        code: number,
        headers: {
            [key: string]: string,
        },
        body?: string,
    } {
        const url: string = `${this.baseUrl}/pet/findByTags?tags=${tags}`;

        const { code, headers: resHeaders, body } = http.get(url);

        return { code, headers: resHeaders };
    }
    public getPetById(petId: number): {
        code: number,
        headers: {
            [key: string]: string,
        },
        body?: string,
    } {
        const url: string = `${this.baseUrl}/pet/${petId}`;

        const { code, headers: resHeaders, body } = http.get(url);

        return { code, headers: resHeaders };
    }
    public updatePet(pet: Pet): {
        code: number,
        headers: {
            [key: string]: string,
        },
        body?: string,
    } {
        const url: string = `${this.baseUrl}/pet`;

        const reqHeaders = {
            'Content-Type': `application/json`
        };

        const { code, headers: resHeaders, body } = http.put(url, pet, { headers: reqHeaders });

        return { code, headers: resHeaders };
    }
    public updatePetWithForm(petId: number, name: string, status: string): {
        code: number,
        headers: {
            [key: string]: string,
        },
        body?: string,
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

        return { code, headers: resHeaders };
    }
    public uploadFile(petId: number, additionalMetadata: string, file: FileData): {
        code: number,
        headers: {
            [key: string]: string,
        },
        body?: string,
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

        return { code, headers: resHeaders };
    }
}
