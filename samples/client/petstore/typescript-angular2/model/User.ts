'use strict';
import * as model from "./model.d.ts"

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

