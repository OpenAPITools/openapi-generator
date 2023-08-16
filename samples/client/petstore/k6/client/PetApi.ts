// @ts-ignore
import http from 'k6/http';
// @ts-ignore
import { FileData } from 'k6/http';

import { Pet } from './Pet.js';

export class PetApi {

    constructor(private baseUrl: string) {}

    public addPet(pet: Pet) {
        const url: string = `${this.baseUrl}/pet`;

        const headers = {
            'Content-Type': ``
        };

        return http.post(url, pet);
    }
    public deletePet(petId: number, apiKey: string) {
        const url: string = `${this.baseUrl}/pet/${petId}`;

        return http.delete(url);
    }
    public findPetsByStatus(status: any[]) {
        const url: string = `${this.baseUrl}/pet/findByStatus?status=${status}`;

        return http.get(url);
    }
    public findPetsByTags(tags: any[]) {
        const url: string = `${this.baseUrl}/pet/findByTags?tags=${tags}`;

        return http.get(url);
    }
    public getPetById(petId: number) {
        const url: string = `${this.baseUrl}/pet/${petId}`;

        return http.get(url);
    }
    public updatePet(pet: Pet) {
        const url: string = `${this.baseUrl}/pet`;

        const headers = {
            'Content-Type': ``
        };

        return http.put(url, pet);
    }
    public updatePetWithForm(petId: number, name: string, status: string) {
        const url: string = `${this.baseUrl}/pet/${petId}`;

        const form = {
            name,
            status,
        };

        return http.post(url, form);
    }
    public uploadFile(petId: number, additionalMetadata: string, file: FileData) {
        const url: string = `${this.baseUrl}/pet/${petId}/uploadImage`;

        const form = {
            additionalMetadata,
            file,
        };

        return http.post(url, form);
    }
}
