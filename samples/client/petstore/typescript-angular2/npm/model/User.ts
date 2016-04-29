'use strict';
import * as models from './models';

export interface User {

    id?: number;

    username?: string;

    firstName?: string;

    lastName?: string;

    email?: string;

    password?: string;

    phone?: string;

    /**
     * User Status
     */
    userStatus?: number;
}

