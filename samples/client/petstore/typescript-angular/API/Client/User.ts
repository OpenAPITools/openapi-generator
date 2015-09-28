/// <reference path="api.d.ts" />

module API.Client {
    'use strict';

    export class User {

        id: number;

        username: string;

        firstName: string;

        lastName: string;

        email: string;

        password: string;

        phone: string;

        /**
         * User Status
         */
        userStatus: number;
    }

}