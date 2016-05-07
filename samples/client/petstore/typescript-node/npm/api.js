var request = require('request');
var promise = require('bluebird');
var defaultBasePath = 'http://petstore.swagger.io/v2';
var Category = (function () {
    function Category() {
    }
    return Category;
})();
exports.Category = Category;
var Order = (function () {
    function Order() {
    }
    return Order;
})();
exports.Order = Order;
var Order;
(function (Order) {
    (function (StatusEnum) {
        StatusEnum[StatusEnum["StatusEnum_placed"] = 'placed'] = "StatusEnum_placed";
        StatusEnum[StatusEnum["StatusEnum_approved"] = 'approved'] = "StatusEnum_approved";
        StatusEnum[StatusEnum["StatusEnum_delivered"] = 'delivered'] = "StatusEnum_delivered";
    })(Order.StatusEnum || (Order.StatusEnum = {}));
    var StatusEnum = Order.StatusEnum;
})(Order = exports.Order || (exports.Order = {}));
var Pet = (function () {
    function Pet() {
    }
    return Pet;
})();
exports.Pet = Pet;
var Pet;
(function (Pet) {
    (function (StatusEnum) {
        StatusEnum[StatusEnum["StatusEnum_available"] = 'available'] = "StatusEnum_available";
        StatusEnum[StatusEnum["StatusEnum_pending"] = 'pending'] = "StatusEnum_pending";
        StatusEnum[StatusEnum["StatusEnum_sold"] = 'sold'] = "StatusEnum_sold";
    })(Pet.StatusEnum || (Pet.StatusEnum = {}));
    var StatusEnum = Pet.StatusEnum;
})(Pet = exports.Pet || (exports.Pet = {}));
var Tag = (function () {
    function Tag() {
    }
    return Tag;
})();
exports.Tag = Tag;
var User = (function () {
    function User() {
    }
    return User;
})();
exports.User = User;
var HttpBasicAuth = (function () {
    function HttpBasicAuth() {
    }
    HttpBasicAuth.prototype.applyToRequest = function (requestOptions) {
        requestOptions.auth = {
            username: this.username, password: this.password
        };
    };
    return HttpBasicAuth;
})();
exports.HttpBasicAuth = HttpBasicAuth;
var ApiKeyAuth = (function () {
    function ApiKeyAuth(location, paramName) {
        this.location = location;
        this.paramName = paramName;
    }
    ApiKeyAuth.prototype.applyToRequest = function (requestOptions) {
        if (this.location == "query") {
            requestOptions.qs[this.paramName] = this.apiKey;
        }
        else if (this.location == "header") {
            requestOptions.headers[this.paramName] = this.apiKey;
        }
    };
    return ApiKeyAuth;
})();
exports.ApiKeyAuth = ApiKeyAuth;
var OAuth = (function () {
    function OAuth() {
    }
    OAuth.prototype.applyToRequest = function (requestOptions) {
        requestOptions.headers["Authorization"] = "Bearer " + this.accessToken;
    };
    return OAuth;
})();
exports.OAuth = OAuth;
var VoidAuth = (function () {
    function VoidAuth() {
    }
    VoidAuth.prototype.applyToRequest = function (requestOptions) {
    };
    return VoidAuth;
})();
exports.VoidAuth = VoidAuth;
(function (PetApiApiKeys) {
    PetApiApiKeys[PetApiApiKeys["api_key"] = 0] = "api_key";
})(exports.PetApiApiKeys || (exports.PetApiApiKeys = {}));
var PetApiApiKeys = exports.PetApiApiKeys;
var PetApi = (function () {
    function PetApi(basePathOrUsername, password, basePath) {
        this.basePath = defaultBasePath;
        this.defaultHeaders = {};
        this.authentications = {
            'default': new VoidAuth(),
            'api_key': new ApiKeyAuth('header', 'api_key'),
            'petstore_auth': new OAuth(),
        };
        if (password) {
            if (basePath) {
                this.basePath = basePath;
            }
        }
        else {
            if (basePathOrUsername) {
                this.basePath = basePathOrUsername;
            }
        }
    }
    PetApi.prototype.setApiKey = function (key, value) {
        this.authentications[PetApiApiKeys[key]].apiKey = value;
    };
    Object.defineProperty(PetApi.prototype, "accessToken", {
        set: function (token) {
            this.authentications.petstore_auth.accessToken = token;
        },
        enumerable: true,
        configurable: true
    });
    PetApi.prototype.extendObj = function (objA, objB) {
        for (var key in objB) {
            if (objB.hasOwnProperty(key)) {
                objA[key] = objB[key];
            }
        }
        return objA;
    };
    PetApi.prototype.addPet = function (body) {
        var localVarPath = this.basePath + '/pet';
        var queryParameters = {};
        var headerParams = this.extendObj({}, this.defaultHeaders);
        var formParams = {};
        var useFormData = false;
        var localVarDeferred = promise.defer();
        var requestOptions = {
            method: 'POST',
            qs: queryParameters,
            headers: headerParams,
            uri: localVarPath,
            json: true,
            body: body,
        };
        this.authentications.petstore_auth.applyToRequest(requestOptions);
        this.authentications.default.applyToRequest(requestOptions);
        if (Object.keys(formParams).length) {
            if (useFormData) {
                requestOptions.formData = formParams;
            }
            else {
                requestOptions.form = formParams;
            }
        }
        request(requestOptions, function (error, response, body) {
            if (error) {
                localVarDeferred.reject(error);
            }
            else {
                if (response.statusCode >= 200 && response.statusCode <= 299) {
                    localVarDeferred.resolve({ response: response, body: body });
                }
                else {
                    localVarDeferred.reject({ response: response, body: body });
                }
            }
        });
        return localVarDeferred.promise;
    };
    PetApi.prototype.deletePet = function (petId, apiKey) {
        var localVarPath = this.basePath + '/pet/{petId}'
            .replace('{' + 'petId' + '}', String(petId));
        var queryParameters = {};
        var headerParams = this.extendObj({}, this.defaultHeaders);
        var formParams = {};
        if (petId === null || petId === undefined) {
            throw new Error('Required parameter petId was null or undefined when calling deletePet.');
        }
        headerParams['api_key'] = apiKey;
        var useFormData = false;
        var localVarDeferred = promise.defer();
        var requestOptions = {
            method: 'DELETE',
            qs: queryParameters,
            headers: headerParams,
            uri: localVarPath,
            json: true,
        };
        this.authentications.petstore_auth.applyToRequest(requestOptions);
        this.authentications.default.applyToRequest(requestOptions);
        if (Object.keys(formParams).length) {
            if (useFormData) {
                requestOptions.formData = formParams;
            }
            else {
                requestOptions.form = formParams;
            }
        }
        request(requestOptions, function (error, response, body) {
            if (error) {
                localVarDeferred.reject(error);
            }
            else {
                if (response.statusCode >= 200 && response.statusCode <= 299) {
                    localVarDeferred.resolve({ response: response, body: body });
                }
                else {
                    localVarDeferred.reject({ response: response, body: body });
                }
            }
        });
        return localVarDeferred.promise;
    };
    PetApi.prototype.findPetsByStatus = function (status) {
        var localVarPath = this.basePath + '/pet/findByStatus';
        var queryParameters = {};
        var headerParams = this.extendObj({}, this.defaultHeaders);
        var formParams = {};
        if (status !== undefined) {
            queryParameters['status'] = status;
        }
        var useFormData = false;
        var localVarDeferred = promise.defer();
        var requestOptions = {
            method: 'GET',
            qs: queryParameters,
            headers: headerParams,
            uri: localVarPath,
            json: true,
        };
        this.authentications.petstore_auth.applyToRequest(requestOptions);
        this.authentications.default.applyToRequest(requestOptions);
        if (Object.keys(formParams).length) {
            if (useFormData) {
                requestOptions.formData = formParams;
            }
            else {
                requestOptions.form = formParams;
            }
        }
        request(requestOptions, function (error, response, body) {
            if (error) {
                localVarDeferred.reject(error);
            }
            else {
                if (response.statusCode >= 200 && response.statusCode <= 299) {
                    localVarDeferred.resolve({ response: response, body: body });
                }
                else {
                    localVarDeferred.reject({ response: response, body: body });
                }
            }
        });
        return localVarDeferred.promise;
    };
    PetApi.prototype.findPetsByTags = function (tags) {
        var localVarPath = this.basePath + '/pet/findByTags';
        var queryParameters = {};
        var headerParams = this.extendObj({}, this.defaultHeaders);
        var formParams = {};
        if (tags !== undefined) {
            queryParameters['tags'] = tags;
        }
        var useFormData = false;
        var localVarDeferred = promise.defer();
        var requestOptions = {
            method: 'GET',
            qs: queryParameters,
            headers: headerParams,
            uri: localVarPath,
            json: true,
        };
        this.authentications.petstore_auth.applyToRequest(requestOptions);
        this.authentications.default.applyToRequest(requestOptions);
        if (Object.keys(formParams).length) {
            if (useFormData) {
                requestOptions.formData = formParams;
            }
            else {
                requestOptions.form = formParams;
            }
        }
        request(requestOptions, function (error, response, body) {
            if (error) {
                localVarDeferred.reject(error);
            }
            else {
                if (response.statusCode >= 200 && response.statusCode <= 299) {
                    localVarDeferred.resolve({ response: response, body: body });
                }
                else {
                    localVarDeferred.reject({ response: response, body: body });
                }
            }
        });
        return localVarDeferred.promise;
    };
    PetApi.prototype.getPetById = function (petId) {
        var localVarPath = this.basePath + '/pet/{petId}'
            .replace('{' + 'petId' + '}', String(petId));
        var queryParameters = {};
        var headerParams = this.extendObj({}, this.defaultHeaders);
        var formParams = {};
        if (petId === null || petId === undefined) {
            throw new Error('Required parameter petId was null or undefined when calling getPetById.');
        }
        var useFormData = false;
        var localVarDeferred = promise.defer();
        var requestOptions = {
            method: 'GET',
            qs: queryParameters,
            headers: headerParams,
            uri: localVarPath,
            json: true,
        };
        this.authentications.api_key.applyToRequest(requestOptions);
        this.authentications.petstore_auth.applyToRequest(requestOptions);
        this.authentications.default.applyToRequest(requestOptions);
        if (Object.keys(formParams).length) {
            if (useFormData) {
                requestOptions.formData = formParams;
            }
            else {
                requestOptions.form = formParams;
            }
        }
        request(requestOptions, function (error, response, body) {
            if (error) {
                localVarDeferred.reject(error);
            }
            else {
                if (response.statusCode >= 200 && response.statusCode <= 299) {
                    localVarDeferred.resolve({ response: response, body: body });
                }
                else {
                    localVarDeferred.reject({ response: response, body: body });
                }
            }
        });
        return localVarDeferred.promise;
    };
    PetApi.prototype.updatePet = function (body) {
        var localVarPath = this.basePath + '/pet';
        var queryParameters = {};
        var headerParams = this.extendObj({}, this.defaultHeaders);
        var formParams = {};
        var useFormData = false;
        var localVarDeferred = promise.defer();
        var requestOptions = {
            method: 'PUT',
            qs: queryParameters,
            headers: headerParams,
            uri: localVarPath,
            json: true,
            body: body,
        };
        this.authentications.petstore_auth.applyToRequest(requestOptions);
        this.authentications.default.applyToRequest(requestOptions);
        if (Object.keys(formParams).length) {
            if (useFormData) {
                requestOptions.formData = formParams;
            }
            else {
                requestOptions.form = formParams;
            }
        }
        request(requestOptions, function (error, response, body) {
            if (error) {
                localVarDeferred.reject(error);
            }
            else {
                if (response.statusCode >= 200 && response.statusCode <= 299) {
                    localVarDeferred.resolve({ response: response, body: body });
                }
                else {
                    localVarDeferred.reject({ response: response, body: body });
                }
            }
        });
        return localVarDeferred.promise;
    };
    PetApi.prototype.updatePetWithForm = function (petId, name, status) {
        var localVarPath = this.basePath + '/pet/{petId}'
            .replace('{' + 'petId' + '}', String(petId));
        var queryParameters = {};
        var headerParams = this.extendObj({}, this.defaultHeaders);
        var formParams = {};
        if (petId === null || petId === undefined) {
            throw new Error('Required parameter petId was null or undefined when calling updatePetWithForm.');
        }
        var useFormData = false;
        if (name !== undefined) {
            formParams['name'] = name;
        }
        if (status !== undefined) {
            formParams['status'] = status;
        }
        var localVarDeferred = promise.defer();
        var requestOptions = {
            method: 'POST',
            qs: queryParameters,
            headers: headerParams,
            uri: localVarPath,
            json: true,
        };
        this.authentications.petstore_auth.applyToRequest(requestOptions);
        this.authentications.default.applyToRequest(requestOptions);
        if (Object.keys(formParams).length) {
            if (useFormData) {
                requestOptions.formData = formParams;
            }
            else {
                requestOptions.form = formParams;
            }
        }
        request(requestOptions, function (error, response, body) {
            if (error) {
                localVarDeferred.reject(error);
            }
            else {
                if (response.statusCode >= 200 && response.statusCode <= 299) {
                    localVarDeferred.resolve({ response: response, body: body });
                }
                else {
                    localVarDeferred.reject({ response: response, body: body });
                }
            }
        });
        return localVarDeferred.promise;
    };
    PetApi.prototype.uploadFile = function (petId, additionalMetadata, file) {
        var localVarPath = this.basePath + '/pet/{petId}/uploadImage'
            .replace('{' + 'petId' + '}', String(petId));
        var queryParameters = {};
        var headerParams = this.extendObj({}, this.defaultHeaders);
        var formParams = {};
        if (petId === null || petId === undefined) {
            throw new Error('Required parameter petId was null or undefined when calling uploadFile.');
        }
        var useFormData = false;
        if (additionalMetadata !== undefined) {
            formParams['additionalMetadata'] = additionalMetadata;
        }
        if (file !== undefined) {
            formParams['file'] = file;
        }
        useFormData = true;
        var localVarDeferred = promise.defer();
        var requestOptions = {
            method: 'POST',
            qs: queryParameters,
            headers: headerParams,
            uri: localVarPath,
            json: true,
        };
        this.authentications.petstore_auth.applyToRequest(requestOptions);
        this.authentications.default.applyToRequest(requestOptions);
        if (Object.keys(formParams).length) {
            if (useFormData) {
                requestOptions.formData = formParams;
            }
            else {
                requestOptions.form = formParams;
            }
        }
        request(requestOptions, function (error, response, body) {
            if (error) {
                localVarDeferred.reject(error);
            }
            else {
                if (response.statusCode >= 200 && response.statusCode <= 299) {
                    localVarDeferred.resolve({ response: response, body: body });
                }
                else {
                    localVarDeferred.reject({ response: response, body: body });
                }
            }
        });
        return localVarDeferred.promise;
    };
    return PetApi;
})();
exports.PetApi = PetApi;
(function (StoreApiApiKeys) {
    StoreApiApiKeys[StoreApiApiKeys["api_key"] = 0] = "api_key";
})(exports.StoreApiApiKeys || (exports.StoreApiApiKeys = {}));
var StoreApiApiKeys = exports.StoreApiApiKeys;
var StoreApi = (function () {
    function StoreApi(basePathOrUsername, password, basePath) {
        this.basePath = defaultBasePath;
        this.defaultHeaders = {};
        this.authentications = {
            'default': new VoidAuth(),
            'api_key': new ApiKeyAuth('header', 'api_key'),
            'petstore_auth': new OAuth(),
        };
        if (password) {
            if (basePath) {
                this.basePath = basePath;
            }
        }
        else {
            if (basePathOrUsername) {
                this.basePath = basePathOrUsername;
            }
        }
    }
    StoreApi.prototype.setApiKey = function (key, value) {
        this.authentications[StoreApiApiKeys[key]].apiKey = value;
    };
    Object.defineProperty(StoreApi.prototype, "accessToken", {
        set: function (token) {
            this.authentications.petstore_auth.accessToken = token;
        },
        enumerable: true,
        configurable: true
    });
    StoreApi.prototype.extendObj = function (objA, objB) {
        for (var key in objB) {
            if (objB.hasOwnProperty(key)) {
                objA[key] = objB[key];
            }
        }
        return objA;
    };
    StoreApi.prototype.deleteOrder = function (orderId) {
        var localVarPath = this.basePath + '/store/order/{orderId}'
            .replace('{' + 'orderId' + '}', String(orderId));
        var queryParameters = {};
        var headerParams = this.extendObj({}, this.defaultHeaders);
        var formParams = {};
        if (orderId === null || orderId === undefined) {
            throw new Error('Required parameter orderId was null or undefined when calling deleteOrder.');
        }
        var useFormData = false;
        var localVarDeferred = promise.defer();
        var requestOptions = {
            method: 'DELETE',
            qs: queryParameters,
            headers: headerParams,
            uri: localVarPath,
            json: true,
        };
        this.authentications.default.applyToRequest(requestOptions);
        if (Object.keys(formParams).length) {
            if (useFormData) {
                requestOptions.formData = formParams;
            }
            else {
                requestOptions.form = formParams;
            }
        }
        request(requestOptions, function (error, response, body) {
            if (error) {
                localVarDeferred.reject(error);
            }
            else {
                if (response.statusCode >= 200 && response.statusCode <= 299) {
                    localVarDeferred.resolve({ response: response, body: body });
                }
                else {
                    localVarDeferred.reject({ response: response, body: body });
                }
            }
        });
        return localVarDeferred.promise;
    };
    StoreApi.prototype.getInventory = function () {
        var localVarPath = this.basePath + '/store/inventory';
        var queryParameters = {};
        var headerParams = this.extendObj({}, this.defaultHeaders);
        var formParams = {};
        var useFormData = false;
        var localVarDeferred = promise.defer();
        var requestOptions = {
            method: 'GET',
            qs: queryParameters,
            headers: headerParams,
            uri: localVarPath,
            json: true,
        };
        this.authentications.api_key.applyToRequest(requestOptions);
        this.authentications.default.applyToRequest(requestOptions);
        if (Object.keys(formParams).length) {
            if (useFormData) {
                requestOptions.formData = formParams;
            }
            else {
                requestOptions.form = formParams;
            }
        }
        request(requestOptions, function (error, response, body) {
            if (error) {
                localVarDeferred.reject(error);
            }
            else {
                if (response.statusCode >= 200 && response.statusCode <= 299) {
                    localVarDeferred.resolve({ response: response, body: body });
                }
                else {
                    localVarDeferred.reject({ response: response, body: body });
                }
            }
        });
        return localVarDeferred.promise;
    };
    StoreApi.prototype.getOrderById = function (orderId) {
        var localVarPath = this.basePath + '/store/order/{orderId}'
            .replace('{' + 'orderId' + '}', String(orderId));
        var queryParameters = {};
        var headerParams = this.extendObj({}, this.defaultHeaders);
        var formParams = {};
        if (orderId === null || orderId === undefined) {
            throw new Error('Required parameter orderId was null or undefined when calling getOrderById.');
        }
        var useFormData = false;
        var localVarDeferred = promise.defer();
        var requestOptions = {
            method: 'GET',
            qs: queryParameters,
            headers: headerParams,
            uri: localVarPath,
            json: true,
        };
        this.authentications.default.applyToRequest(requestOptions);
        if (Object.keys(formParams).length) {
            if (useFormData) {
                requestOptions.formData = formParams;
            }
            else {
                requestOptions.form = formParams;
            }
        }
        request(requestOptions, function (error, response, body) {
            if (error) {
                localVarDeferred.reject(error);
            }
            else {
                if (response.statusCode >= 200 && response.statusCode <= 299) {
                    localVarDeferred.resolve({ response: response, body: body });
                }
                else {
                    localVarDeferred.reject({ response: response, body: body });
                }
            }
        });
        return localVarDeferred.promise;
    };
    StoreApi.prototype.placeOrder = function (body) {
        var localVarPath = this.basePath + '/store/order';
        var queryParameters = {};
        var headerParams = this.extendObj({}, this.defaultHeaders);
        var formParams = {};
        var useFormData = false;
        var localVarDeferred = promise.defer();
        var requestOptions = {
            method: 'POST',
            qs: queryParameters,
            headers: headerParams,
            uri: localVarPath,
            json: true,
            body: body,
        };
        this.authentications.default.applyToRequest(requestOptions);
        if (Object.keys(formParams).length) {
            if (useFormData) {
                requestOptions.formData = formParams;
            }
            else {
                requestOptions.form = formParams;
            }
        }
        request(requestOptions, function (error, response, body) {
            if (error) {
                localVarDeferred.reject(error);
            }
            else {
                if (response.statusCode >= 200 && response.statusCode <= 299) {
                    localVarDeferred.resolve({ response: response, body: body });
                }
                else {
                    localVarDeferred.reject({ response: response, body: body });
                }
            }
        });
        return localVarDeferred.promise;
    };
    return StoreApi;
})();
exports.StoreApi = StoreApi;
(function (UserApiApiKeys) {
    UserApiApiKeys[UserApiApiKeys["api_key"] = 0] = "api_key";
})(exports.UserApiApiKeys || (exports.UserApiApiKeys = {}));
var UserApiApiKeys = exports.UserApiApiKeys;
var UserApi = (function () {
    function UserApi(basePathOrUsername, password, basePath) {
        this.basePath = defaultBasePath;
        this.defaultHeaders = {};
        this.authentications = {
            'default': new VoidAuth(),
            'api_key': new ApiKeyAuth('header', 'api_key'),
            'petstore_auth': new OAuth(),
        };
        if (password) {
            if (basePath) {
                this.basePath = basePath;
            }
        }
        else {
            if (basePathOrUsername) {
                this.basePath = basePathOrUsername;
            }
        }
    }
    UserApi.prototype.setApiKey = function (key, value) {
        this.authentications[UserApiApiKeys[key]].apiKey = value;
    };
    Object.defineProperty(UserApi.prototype, "accessToken", {
        set: function (token) {
            this.authentications.petstore_auth.accessToken = token;
        },
        enumerable: true,
        configurable: true
    });
    UserApi.prototype.extendObj = function (objA, objB) {
        for (var key in objB) {
            if (objB.hasOwnProperty(key)) {
                objA[key] = objB[key];
            }
        }
        return objA;
    };
    UserApi.prototype.createUser = function (body) {
        var localVarPath = this.basePath + '/user';
        var queryParameters = {};
        var headerParams = this.extendObj({}, this.defaultHeaders);
        var formParams = {};
        var useFormData = false;
        var localVarDeferred = promise.defer();
        var requestOptions = {
            method: 'POST',
            qs: queryParameters,
            headers: headerParams,
            uri: localVarPath,
            json: true,
            body: body,
        };
        this.authentications.default.applyToRequest(requestOptions);
        if (Object.keys(formParams).length) {
            if (useFormData) {
                requestOptions.formData = formParams;
            }
            else {
                requestOptions.form = formParams;
            }
        }
        request(requestOptions, function (error, response, body) {
            if (error) {
                localVarDeferred.reject(error);
            }
            else {
                if (response.statusCode >= 200 && response.statusCode <= 299) {
                    localVarDeferred.resolve({ response: response, body: body });
                }
                else {
                    localVarDeferred.reject({ response: response, body: body });
                }
            }
        });
        return localVarDeferred.promise;
    };
    UserApi.prototype.createUsersWithArrayInput = function (body) {
        var localVarPath = this.basePath + '/user/createWithArray';
        var queryParameters = {};
        var headerParams = this.extendObj({}, this.defaultHeaders);
        var formParams = {};
        var useFormData = false;
        var localVarDeferred = promise.defer();
        var requestOptions = {
            method: 'POST',
            qs: queryParameters,
            headers: headerParams,
            uri: localVarPath,
            json: true,
            body: body,
        };
        this.authentications.default.applyToRequest(requestOptions);
        if (Object.keys(formParams).length) {
            if (useFormData) {
                requestOptions.formData = formParams;
            }
            else {
                requestOptions.form = formParams;
            }
        }
        request(requestOptions, function (error, response, body) {
            if (error) {
                localVarDeferred.reject(error);
            }
            else {
                if (response.statusCode >= 200 && response.statusCode <= 299) {
                    localVarDeferred.resolve({ response: response, body: body });
                }
                else {
                    localVarDeferred.reject({ response: response, body: body });
                }
            }
        });
        return localVarDeferred.promise;
    };
    UserApi.prototype.createUsersWithListInput = function (body) {
        var localVarPath = this.basePath + '/user/createWithList';
        var queryParameters = {};
        var headerParams = this.extendObj({}, this.defaultHeaders);
        var formParams = {};
        var useFormData = false;
        var localVarDeferred = promise.defer();
        var requestOptions = {
            method: 'POST',
            qs: queryParameters,
            headers: headerParams,
            uri: localVarPath,
            json: true,
            body: body,
        };
        this.authentications.default.applyToRequest(requestOptions);
        if (Object.keys(formParams).length) {
            if (useFormData) {
                requestOptions.formData = formParams;
            }
            else {
                requestOptions.form = formParams;
            }
        }
        request(requestOptions, function (error, response, body) {
            if (error) {
                localVarDeferred.reject(error);
            }
            else {
                if (response.statusCode >= 200 && response.statusCode <= 299) {
                    localVarDeferred.resolve({ response: response, body: body });
                }
                else {
                    localVarDeferred.reject({ response: response, body: body });
                }
            }
        });
        return localVarDeferred.promise;
    };
    UserApi.prototype.deleteUser = function (username) {
        var localVarPath = this.basePath + '/user/{username}'
            .replace('{' + 'username' + '}', String(username));
        var queryParameters = {};
        var headerParams = this.extendObj({}, this.defaultHeaders);
        var formParams = {};
        if (username === null || username === undefined) {
            throw new Error('Required parameter username was null or undefined when calling deleteUser.');
        }
        var useFormData = false;
        var localVarDeferred = promise.defer();
        var requestOptions = {
            method: 'DELETE',
            qs: queryParameters,
            headers: headerParams,
            uri: localVarPath,
            json: true,
        };
        this.authentications.default.applyToRequest(requestOptions);
        if (Object.keys(formParams).length) {
            if (useFormData) {
                requestOptions.formData = formParams;
            }
            else {
                requestOptions.form = formParams;
            }
        }
        request(requestOptions, function (error, response, body) {
            if (error) {
                localVarDeferred.reject(error);
            }
            else {
                if (response.statusCode >= 200 && response.statusCode <= 299) {
                    localVarDeferred.resolve({ response: response, body: body });
                }
                else {
                    localVarDeferred.reject({ response: response, body: body });
                }
            }
        });
        return localVarDeferred.promise;
    };
    UserApi.prototype.getUserByName = function (username) {
        var localVarPath = this.basePath + '/user/{username}'
            .replace('{' + 'username' + '}', String(username));
        var queryParameters = {};
        var headerParams = this.extendObj({}, this.defaultHeaders);
        var formParams = {};
        if (username === null || username === undefined) {
            throw new Error('Required parameter username was null or undefined when calling getUserByName.');
        }
        var useFormData = false;
        var localVarDeferred = promise.defer();
        var requestOptions = {
            method: 'GET',
            qs: queryParameters,
            headers: headerParams,
            uri: localVarPath,
            json: true,
        };
        this.authentications.default.applyToRequest(requestOptions);
        if (Object.keys(formParams).length) {
            if (useFormData) {
                requestOptions.formData = formParams;
            }
            else {
                requestOptions.form = formParams;
            }
        }
        request(requestOptions, function (error, response, body) {
            if (error) {
                localVarDeferred.reject(error);
            }
            else {
                if (response.statusCode >= 200 && response.statusCode <= 299) {
                    localVarDeferred.resolve({ response: response, body: body });
                }
                else {
                    localVarDeferred.reject({ response: response, body: body });
                }
            }
        });
        return localVarDeferred.promise;
    };
    UserApi.prototype.loginUser = function (username, password) {
        var localVarPath = this.basePath + '/user/login';
        var queryParameters = {};
        var headerParams = this.extendObj({}, this.defaultHeaders);
        var formParams = {};
        if (username !== undefined) {
            queryParameters['username'] = username;
        }
        if (password !== undefined) {
            queryParameters['password'] = password;
        }
        var useFormData = false;
        var localVarDeferred = promise.defer();
        var requestOptions = {
            method: 'GET',
            qs: queryParameters,
            headers: headerParams,
            uri: localVarPath,
            json: true,
        };
        this.authentications.default.applyToRequest(requestOptions);
        if (Object.keys(formParams).length) {
            if (useFormData) {
                requestOptions.formData = formParams;
            }
            else {
                requestOptions.form = formParams;
            }
        }
        request(requestOptions, function (error, response, body) {
            if (error) {
                localVarDeferred.reject(error);
            }
            else {
                if (response.statusCode >= 200 && response.statusCode <= 299) {
                    localVarDeferred.resolve({ response: response, body: body });
                }
                else {
                    localVarDeferred.reject({ response: response, body: body });
                }
            }
        });
        return localVarDeferred.promise;
    };
    UserApi.prototype.logoutUser = function () {
        var localVarPath = this.basePath + '/user/logout';
        var queryParameters = {};
        var headerParams = this.extendObj({}, this.defaultHeaders);
        var formParams = {};
        var useFormData = false;
        var localVarDeferred = promise.defer();
        var requestOptions = {
            method: 'GET',
            qs: queryParameters,
            headers: headerParams,
            uri: localVarPath,
            json: true,
        };
        this.authentications.default.applyToRequest(requestOptions);
        if (Object.keys(formParams).length) {
            if (useFormData) {
                requestOptions.formData = formParams;
            }
            else {
                requestOptions.form = formParams;
            }
        }
        request(requestOptions, function (error, response, body) {
            if (error) {
                localVarDeferred.reject(error);
            }
            else {
                if (response.statusCode >= 200 && response.statusCode <= 299) {
                    localVarDeferred.resolve({ response: response, body: body });
                }
                else {
                    localVarDeferred.reject({ response: response, body: body });
                }
            }
        });
        return localVarDeferred.promise;
    };
    UserApi.prototype.updateUser = function (username, body) {
        var localVarPath = this.basePath + '/user/{username}'
            .replace('{' + 'username' + '}', String(username));
        var queryParameters = {};
        var headerParams = this.extendObj({}, this.defaultHeaders);
        var formParams = {};
        if (username === null || username === undefined) {
            throw new Error('Required parameter username was null or undefined when calling updateUser.');
        }
        var useFormData = false;
        var localVarDeferred = promise.defer();
        var requestOptions = {
            method: 'PUT',
            qs: queryParameters,
            headers: headerParams,
            uri: localVarPath,
            json: true,
            body: body,
        };
        this.authentications.default.applyToRequest(requestOptions);
        if (Object.keys(formParams).length) {
            if (useFormData) {
                requestOptions.formData = formParams;
            }
            else {
                requestOptions.form = formParams;
            }
        }
        request(requestOptions, function (error, response, body) {
            if (error) {
                localVarDeferred.reject(error);
            }
            else {
                if (response.statusCode >= 200 && response.statusCode <= 299) {
                    localVarDeferred.resolve({ response: response, body: body });
                }
                else {
                    localVarDeferred.reject({ response: response, body: body });
                }
            }
        });
        return localVarDeferred.promise;
    };
    return UserApi;
})();
exports.UserApi = UserApi;
//# sourceMappingURL=api.js.map