// @ts-ignore
import http from 'k6/http';
// @ts-ignore
import { FileData } from 'k6/http';

import { User } from './User.js';

export class UserApi {

    constructor(private baseUrl: string) {}

    public createUser(user: User) {
        const url: string = `${this.baseUrl}/user`;

        const headers = {
            'Content-Type': `application/json`
        };

        return http.post(url, user, { headers });
    }
    public createUsersWithArrayInput(user: any[]) {
        const url: string = `${this.baseUrl}/user/createWithArray`;

        const headers = {
            'Content-Type': `application/json`
        };

        return http.post(url, user, { headers });
    }
    public createUsersWithListInput(user: any[]) {
        const url: string = `${this.baseUrl}/user/createWithList`;

        const headers = {
            'Content-Type': `application/json`
        };

        return http.post(url, user, { headers });
    }
    public deleteUser(username: string) {
        const url: string = `${this.baseUrl}/user/${username}`;

        return http.delete(url);
    }
    public getUserByName(username: string) {
        const url: string = `${this.baseUrl}/user/${username}`;

        return http.get(url);
    }
    public loginUser(username: string, password: string) {
        const url: string = `${this.baseUrl}/user/login?username=${username}&password=${password}`;

        return http.get(url);
    }
    public logoutUser() {
        const url: string = `${this.baseUrl}/user/logout`;

        return http.get(url);
    }
    public updateUser(username: string, user: User) {
        const url: string = `${this.baseUrl}/user/${username}`;

        const headers = {
            'Content-Type': `application/json`
        };

        return http.put(url, user, { headers });
    }
}
