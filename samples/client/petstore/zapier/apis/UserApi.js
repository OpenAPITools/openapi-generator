const samples = require('../samples/UserApi');
const User = require('../models/User');
const utils = require('../utils/utils');

module.exports = {
    createUser: {
        key: 'createUser',
        noun: 'User',
        display: {
            label: 'createUser',
            description: 'This can only be done by the logged in user.',
            hidden: false,
        },
        operation: {
            inputFields: [
                ...User.fields(),
            ],
            outputFields: [
            ],
            perform: async (z, bundle) => {
                const options = {
                    url: utils.replacePathParameters('http://petstore.swagger.io/v2/user'),
                    method: 'POST',
                    removeMissingValuesFrom: { params: true, body: true },
                    headers: {
                        'Authorization': 'Bearer {{bundle.authData.access_token}}',
                        'Content-Type': 'application/json',
                        'Accept': '',
                    },
                    params: {
                    },
                    body: {
                        ...User.mapping(bundle),
                    },
                }
                return z.request(options).then((response) => {
                    response.throwForStatus();
                    const results = response.json;
                    return results;
                })
            },
            sample: { data: {} }
        }
    },
    createUsersWithArrayInput: {
        key: 'createUsersWithArrayInput',
        noun: 'User',
        display: {
            label: 'createUsersWithArrayInput',
            description: '',
            hidden: false,
        },
        operation: {
            inputFields: [
                ...User.fields(),
            ],
            outputFields: [
            ],
            perform: async (z, bundle) => {
                const options = {
                    url: utils.replacePathParameters('http://petstore.swagger.io/v2/user/createWithArray'),
                    method: 'POST',
                    removeMissingValuesFrom: { params: true, body: true },
                    headers: {
                        'Authorization': 'Bearer {{bundle.authData.access_token}}',
                        'Content-Type': 'application/json',
                        'Accept': '',
                    },
                    params: {
                    },
                    body: {
                        ...User.mapping(bundle),
                    },
                }
                return z.request(options).then((response) => {
                    response.throwForStatus();
                    const results = response.json;
                    return results;
                })
            },
            sample: { data: {} }
        }
    },
    createUsersWithListInput: {
        key: 'createUsersWithListInput',
        noun: 'User',
        display: {
            label: 'createUsersWithListInput',
            description: '',
            hidden: false,
        },
        operation: {
            inputFields: [
                ...User.fields(),
            ],
            outputFields: [
            ],
            perform: async (z, bundle) => {
                const options = {
                    url: utils.replacePathParameters('http://petstore.swagger.io/v2/user/createWithList'),
                    method: 'POST',
                    removeMissingValuesFrom: { params: true, body: true },
                    headers: {
                        'Authorization': 'Bearer {{bundle.authData.access_token}}',
                        'Content-Type': 'application/json',
                        'Accept': '',
                    },
                    params: {
                    },
                    body: {
                        ...User.mapping(bundle),
                    },
                }
                return z.request(options).then((response) => {
                    response.throwForStatus();
                    const results = response.json;
                    return results;
                })
            },
            sample: { data: {} }
        }
    },
    deleteUser: {
        key: 'deleteUser',
        noun: 'User',
        display: {
            label: 'deleteUser',
            description: 'This can only be done by the logged in user.',
            hidden: false,
        },
        operation: {
            inputFields: [
                {
                    key: 'username',
                    label: 'The name that needs to be deleted',
                    type: 'string',
                    required: true,
                },
            ],
            outputFields: [
            ],
            perform: async (z, bundle) => {
                const options = {
                    url: utils.replacePathParameters('http://petstore.swagger.io/v2/user/{username}'),
                    method: 'DELETE',
                    removeMissingValuesFrom: { params: true, body: true },
                    headers: {
                        'Authorization': 'Bearer {{bundle.authData.access_token}}',
                        'Content-Type': '',
                        'Accept': '',
                    },
                    params: {
                    },
                    body: {
                    },
                }
                return z.request(options).then((response) => {
                    response.throwForStatus();
                    const results = response.json;
                    return results;
                })
            },
            sample: { data: {} }
        }
    },
    getUserByName: {
        key: 'getUserByName',
        noun: 'User',
        display: {
            label: 'getUserByName',
            description: '',
            hidden: false,
        },
        operation: {
            inputFields: [
                {
                    key: 'username',
                    label: 'The name that needs to be fetched. Use user1 for testing.',
                    type: 'string',
                    required: true,
                },
            ],
            outputFields: [
                ...User.fields('', false),
            ],
            perform: async (z, bundle) => {
                const options = {
                    url: utils.replacePathParameters('http://petstore.swagger.io/v2/user/{username}'),
                    method: 'GET',
                    removeMissingValuesFrom: { params: true, body: true },
                    headers: {
                        'Authorization': 'Bearer {{bundle.authData.access_token}}',
                        'Content-Type': '',
                        'Accept': 'application/xml, application/json',
                    },
                    params: {
                    },
                    body: {
                    },
                }
                return z.request(options).then((response) => {
                    response.throwForStatus();
                    const results = response.json;
                    return results;
                })
            },
            sample: samples['UserSample']
        }
    },
    loginUser: {
        key: 'loginUser',
        noun: 'User',
        display: {
            label: 'loginUser',
            description: '',
            hidden: false,
        },
        operation: {
            inputFields: [
                {
                    key: 'username',
                    label: 'The user name for login',
                    type: 'string',
                    required: true,
                },
                {
                    key: 'password',
                    label: 'The password for login in clear text',
                    type: 'string',
                    required: true,
                },
            ],
            outputFields: [
            ],
            perform: async (z, bundle) => {
                const options = {
                    url: utils.replacePathParameters('http://petstore.swagger.io/v2/user/login'),
                    method: 'GET',
                    removeMissingValuesFrom: { params: true, body: true },
                    headers: {
                        'Authorization': 'Bearer {{bundle.authData.access_token}}',
                        'Content-Type': '',
                        'Accept': 'application/xml, application/json',
                    },
                    params: {
                        'username': bundle.inputData?.['username'],
                        'password': bundle.inputData?.['password'],
                    },
                    body: {
                    },
                }
                return z.request(options).then((response) => {
                    response.throwForStatus();
                    const results = response.json;
                    return { data: results };
                })
            },
            sample: { data: {} }
        }
    },
    logoutUser: {
        key: 'logoutUser',
        noun: 'User',
        display: {
            label: 'logoutUser',
            description: '',
            hidden: false,
        },
        operation: {
            inputFields: [
            ],
            outputFields: [
            ],
            perform: async (z, bundle) => {
                const options = {
                    url: utils.replacePathParameters('http://petstore.swagger.io/v2/user/logout'),
                    method: 'GET',
                    removeMissingValuesFrom: { params: true, body: true },
                    headers: {
                        'Authorization': 'Bearer {{bundle.authData.access_token}}',
                        'Content-Type': '',
                        'Accept': '',
                    },
                    params: {
                    },
                    body: {
                    },
                }
                return z.request(options).then((response) => {
                    response.throwForStatus();
                    const results = response.json;
                    return results;
                })
            },
            sample: { data: {} }
        }
    },
    updateUser: {
        key: 'updateUser',
        noun: 'User',
        display: {
            label: 'updateUser',
            description: 'This can only be done by the logged in user.',
            hidden: false,
        },
        operation: {
            inputFields: [
                {
                    key: 'username',
                    label: 'name that need to be deleted',
                    type: 'string',
                    required: true,
                },
                ...User.fields(),
            ],
            outputFields: [
            ],
            perform: async (z, bundle) => {
                const options = {
                    url: utils.replacePathParameters('http://petstore.swagger.io/v2/user/{username}'),
                    method: 'PUT',
                    removeMissingValuesFrom: { params: true, body: true },
                    headers: {
                        'Authorization': 'Bearer {{bundle.authData.access_token}}',
                        'Content-Type': 'application/json',
                        'Accept': '',
                    },
                    params: {
                    },
                    body: {
                        ...User.mapping(bundle),
                    },
                }
                return z.request(options).then((response) => {
                    response.throwForStatus();
                    const results = response.json;
                    return results;
                })
            },
            sample: { data: {} }
        }
    },
}
