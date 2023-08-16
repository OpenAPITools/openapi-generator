// @ts-ignore
import http from 'k6/http';
// @ts-ignore
import { FileData } from 'k6/http';

import { User } from './User.js';

export class UserApi {

    constructor(private baseUrl: string) {}

    public createUser(user: User) {
        const url: string = `${this.baseUrl}/user`;
        const payload: string = JSON.stringify(user);

        return http.post(url, payload);
    }
    public createUsersWithArrayInput(user: any[]) {
        const url: string = `${this.baseUrl}/user/createWithArray`;
        const payload: string = JSON.stringify(user);

        return http.post(url, payload);
    }
    public createUsersWithListInput(user: any[]) {
        const url: string = `${this.baseUrl}/user/createWithList`;
        const payload: string = JSON.stringify(user);

        return http.post(url, payload);
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
        const payload: string = JSON.stringify(user);

        return http.put(url, payload);
    }
}
