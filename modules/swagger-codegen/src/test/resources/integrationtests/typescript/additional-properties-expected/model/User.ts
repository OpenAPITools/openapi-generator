'use strict';
import * as models from './models';

export interface User {
    [key: string]: string

    id?: number;

    /**
     * User Status
     */
    userStatus?: number;
}
