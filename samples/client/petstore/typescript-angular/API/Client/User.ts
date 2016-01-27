/// <reference path="api.d.ts" />

namespace API.Client {
    'use strict';

    export interface User {

        "id"?: number;

        "username"?: string;

        "firstName"?: string;

        "lastName"?: string;

        "email"?: string;

        "password"?: string;

        "phone"?: string;

        /**
         * User Status
         */
        "userStatus"?: number;
    }

}
