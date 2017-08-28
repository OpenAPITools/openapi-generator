import * as api from './api/api';
import * as angular from 'angular';

const apiModule = angular.module('api', [])
.service('PetApi', api.PetApi)
.service('StoreApi', api.StoreApi)
.service('UserApi', api.UserApi)

export default apiModule;
