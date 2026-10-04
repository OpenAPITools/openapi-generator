// @ts-ignore
import http from 'k6/http';
// @ts-ignore
import { FileData } from 'k6/http';

import { User } from './User.js';

export class UserApi {

    constructor(private baseUrl: string) {}

    /**
    * @summary Create user
    *
    * @returns { undefined } - 0
    */
    public createUser(user: User): {
        code: number,
        headers: {
            [key: string]: string,
        },
        body?: undefined,
    } {
        const url: string = `${this.baseUrl}/user`;

        const reqHeaders = {
            'Content-Type': `application/json`
        };

        const { code, headers: resHeaders, body } = http.post(url, user, { headers: reqHeaders });

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
    * @summary Creates list of users with given input array
    *
    * @returns { undefined } - 0
    */
    public createUsersWithArrayInput(user: User[]): {
        code: number,
        headers: {
            [key: string]: string,
        },
        body?: undefined,
    } {
        const url: string = `${this.baseUrl}/user/createWithArray`;

        const reqHeaders = {
            'Content-Type': `application/json`
        };

        const { code, headers: resHeaders, body } = http.post(url, user, { headers: reqHeaders });

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
    * @summary Creates list of users with given input array
    *
    * @returns { undefined } - 0
    */
    public createUsersWithListInput(user: User[]): {
        code: number,
        headers: {
            [key: string]: string,
        },
        body?: undefined,
    } {
        const url: string = `${this.baseUrl}/user/createWithList`;

        const reqHeaders = {
            'Content-Type': `application/json`
        };

        const { code, headers: resHeaders, body } = http.post(url, user, { headers: reqHeaders });

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
    * @summary Delete user
    *
    * @returns { undefined } - 400
    * @returns { undefined } - 404
    */
    public deleteUser(username: string): {
        code: number,
        headers: {
            [key: string]: string,
        },
        body?: undefined,
    } {
        const url: string = `${this.baseUrl}/user/${username}`;

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
    * @summary Get user by user name
    *
    * @returns { User } - 200
    * @returns { undefined } - 400
    * @returns { undefined } - 404
    */
    public getUserByName(username: string): {
        code: number,
        headers: {
            [key: string]: string,
        },
        body?: User | undefined,
    } {
        const url: string = `${this.baseUrl}/user/${username}`;

        const { code, headers: resHeaders, body } = http.get(url);

        let responseBody = undefined;

        if (body) {
            try {
                responseBody = JSON.parse(body);
            } catch (error) {
                responseBody = body;
            }
        }

        return { code, headers: resHeaders, body: responseBody as User | undefined };
    }
    /**
    * @summary Logs user into the system
    *
    * @returns { string } - 200
    * @returns { undefined } - 400
    */
    public loginUser(username: string, password: string): {
        code: number,
        headers: {
            [key: string]: string,
        },
        body?: string | undefined,
    } {
        const url: string = `${this.baseUrl}/user/login?username=${username}&password=${password}`;

        const { code, headers: resHeaders, body } = http.get(url);

        let responseBody = undefined;

        if (body) {
            try {
                responseBody = JSON.parse(body);
            } catch (error) {
                responseBody = body;
            }
        }

        return { code, headers: resHeaders, body: responseBody as string | undefined };
    }
    /**
    * @summary Logs out current logged in user session
    *
    * @returns { undefined } - 0
    */
    public logoutUser(): {
        code: number,
        headers: {
            [key: string]: string,
        },
        body?: undefined,
    } {
        const url: string = `${this.baseUrl}/user/logout`;

        const { code, headers: resHeaders, body } = http.get(url);

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
    * @summary Updated user
    *
    * @returns { undefined } - 400
    * @returns { undefined } - 404
    */
    public updateUser(username: string, user: User): {
        code: number,
        headers: {
            [key: string]: string,
        },
        body?: undefined,
    } {
        const url: string = `${this.baseUrl}/user/${username}`;

        const reqHeaders = {
            'Content-Type': `application/json`
        };

        const { code, headers: resHeaders, body } = http.put(url, user, { headers: reqHeaders });

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
}
