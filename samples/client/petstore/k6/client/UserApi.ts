// @ts-ignore
import http from 'k6/http';
// @ts-ignore
import { FileData } from 'k6/http';

import { User } from './User.js';

export class UserApi {

    constructor(private baseUrl: string) {}

    public createUser(user: User): {
        code: number,
        headers: {
            [key: string]: string,
        },
        body?: string,
    } {
        const url: string = `${this.baseUrl}/user`;

        const reqHeaders = {
            'Content-Type': `application/json`
        };

        const { code, headers: resHeaders, body } = http.post(url, user, { headers: reqHeaders });

        return { code, headers: resHeaders };
    }
    public createUsersWithArrayInput(user: any[]): {
        code: number,
        headers: {
            [key: string]: string,
        },
        body?: string,
    } {
        const url: string = `${this.baseUrl}/user/createWithArray`;

        const reqHeaders = {
            'Content-Type': `application/json`
        };

        const { code, headers: resHeaders, body } = http.post(url, user, { headers: reqHeaders });

        return { code, headers: resHeaders };
    }
    public createUsersWithListInput(user: any[]): {
        code: number,
        headers: {
            [key: string]: string,
        },
        body?: string,
    } {
        const url: string = `${this.baseUrl}/user/createWithList`;

        const reqHeaders = {
            'Content-Type': `application/json`
        };

        const { code, headers: resHeaders, body } = http.post(url, user, { headers: reqHeaders });

        return { code, headers: resHeaders };
    }
    public deleteUser(username: string): {
        code: number,
        headers: {
            [key: string]: string,
        },
        body?: string,
    } {
        const url: string = `${this.baseUrl}/user/${username}`;

        const { code, headers: resHeaders, body } = http.delete(url);

        return { code, headers: resHeaders };
    }
    public getUserByName(username: string): {
        code: number,
        headers: {
            [key: string]: string,
        },
        body?: string,
    } {
        const url: string = `${this.baseUrl}/user/${username}`;

        const { code, headers: resHeaders, body } = http.get(url);

        return { code, headers: resHeaders };
    }
    public loginUser(username: string, password: string): {
        code: number,
        headers: {
            [key: string]: string,
        },
        body?: string,
    } {
        const url: string = `${this.baseUrl}/user/login?username=${username}&password=${password}`;

        const { code, headers: resHeaders, body } = http.get(url);

        return { code, headers: resHeaders };
    }
    public logoutUser(): {
        code: number,
        headers: {
            [key: string]: string,
        },
        body?: string,
    } {
        const url: string = `${this.baseUrl}/user/logout`;

        const { code, headers: resHeaders, body } = http.get(url);

        return { code, headers: resHeaders };
    }
    public updateUser(username: string, user: User): {
        code: number,
        headers: {
            [key: string]: string,
        },
        body?: string,
    } {
        const url: string = `${this.baseUrl}/user/${username}`;

        const reqHeaders = {
            'Content-Type': `application/json`
        };

        const { code, headers: resHeaders, body } = http.put(url, user, { headers: reqHeaders });

        return { code, headers: resHeaders };
    }
}
