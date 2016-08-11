'use strict';
import * as models from './models';

export interface User {
    [key: string]: string | any;

    id?: number;

    /**
     * User Status
     */
    userStatus?: number;
}
