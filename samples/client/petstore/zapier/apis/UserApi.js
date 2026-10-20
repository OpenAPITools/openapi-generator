const samples = require('../samples/UserApi');
const User = require('../models/User');
const utils = require('../utils/utils');

module.exports = {
    createUser: {
        key: 'createUser',
        noun: 'user',
        display: {
            label: 'Create user',
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
                        'Content-Type': 'application/json',
                        'Accept': '',
                    },
                    params: {
                    },
                    body: {
                        ...User.mapping(bundle),
                    },
                }
                return z.request(utils.requestOptionsMiddleware(z, bundle, options)).then((response) => {
                    response.throwForStatus();
                    const results = utils.responseOptionsMiddleware(z, bundle, 'createUser', response.json);
                    return results;
                })
            },
            sample: { data: {} }
        }
    },
    createUsersWithArrayInput: {
        key: 'createUsersWithArrayInput',
        noun: 'user',
        display: {
            label: 'Creates list of users with given input array',
            description: '',
            hidden: false,
        },
        operation: {
            inputFields: [
                {
                    key: 'User',
                    label: 'List of user object',
                    type: 'string',
                }
            ],
            outputFields: [
            ],
            perform: async (z, bundle) => {
                const options = {
                    url: utils.replacePathParameters('http://petstore.swagger.io/v2/user/createWithArray'),
                    method: 'POST',
                    removeMissingValuesFrom: { params: true, body: true },
                    headers: {
                        'Content-Type': 'application/json',
                        'Accept': '',
                    },
                    params: {
                    },
                    body: {
                        ...User.mapping(bundle),
                    },
                }
                return z.request(utils.requestOptionsMiddleware(z, bundle, options)).then((response) => {
                    response.throwForStatus();
                    const results = utils.responseOptionsMiddleware(z, bundle, 'createUsersWithArrayInput', response.json);
                    return results;
                })
            },
            sample: { data: {} }
        }
    },
    createUsersWithListInput: {
        key: 'createUsersWithListInput',
        noun: 'user',
        display: {
            label: 'Creates list of users with given input array',
            description: '',
            hidden: false,
        },
        operation: {
            inputFields: [
                {
                    key: 'User',
                    label: 'List of user object',
                    type: 'string',
                }
            ],
            outputFields: [
            ],
            perform: async (z, bundle) => {
                const options = {
                    url: utils.replacePathParameters('http://petstore.swagger.io/v2/user/createWithList'),
                    method: 'POST',
                    removeMissingValuesFrom: { params: true, body: true },
                    headers: {
                        'Content-Type': 'application/json',
                        'Accept': '',
                    },
                    params: {
                    },
                    body: {
                        ...User.mapping(bundle),
                    },
                }
                return z.request(utils.requestOptionsMiddleware(z, bundle, options)).then((response) => {
                    response.throwForStatus();
                    const results = utils.responseOptionsMiddleware(z, bundle, 'createUsersWithListInput', response.json);
                    return results;
                })
            },
            sample: { data: {} }
        }
    },
    deleteUser: {
        key: 'deleteUser',
        noun: 'user',
        display: {
            label: 'Delete user',
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
                        'Content-Type': '',
                        'Accept': '',
                    },
                    params: {
                    },
                    body: {
                    },
                }
                return z.request(utils.requestOptionsMiddleware(z, bundle, options)).then((response) => {
                    response.throwForStatus();
                    const results = utils.responseOptionsMiddleware(z, bundle, 'deleteUser', response.json);
                    return results;
                })
            },
            sample: { data: {} }
        }
    },
    getUserByName: {
        key: 'getUserByName',
        noun: 'user',
        display: {
            label: 'Get user by user name',
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
                        'Content-Type': '',
                        'Accept': 'application/xml, application/json',
                    },
                    params: {
                    },
                    body: {
                    },
                }
                return z.request(utils.requestOptionsMiddleware(z, bundle, options)).then((response) => {
                    response.throwForStatus();
                    const results = utils.responseOptionsMiddleware(z, bundle, 'getUserByName', response.json);
                    return results;
                })
            },
            sample: samples['UserSample']
        }
    },
    loginUser: {
        key: 'loginUser',
        noun: 'user',
        display: {
            label: 'Logs user into the system',
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
                return z.request(utils.requestOptionsMiddleware(z, bundle, options)).then((response) => {
                    response.throwForStatus();
                    const results = utils.responseOptionsMiddleware(z, bundle, 'loginUser', response.json);
                    return { data: results };
                })
            },
            sample: { data: {} }
        }
    },
    logoutUser: {
        key: 'logoutUser',
        noun: 'user',
        display: {
            label: 'Logs out current logged in user session',
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
                        'Content-Type': '',
                        'Accept': '',
                    },
                    params: {
                    },
                    body: {
                    },
                }
                return z.request(utils.requestOptionsMiddleware(z, bundle, options)).then((response) => {
                    response.throwForStatus();
                    const results = utils.responseOptionsMiddleware(z, bundle, 'logoutUser', response.json);
                    return results;
                })
            },
            sample: { data: {} }
        }
    },
    updateUser: {
        key: 'updateUser',
        noun: 'user',
        display: {
            label: 'Updated user',
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
                        'Content-Type': 'application/json',
                        'Accept': '',
                    },
                    params: {
                    },
                    body: {
                        ...User.mapping(bundle),
                    },
                }
                return z.request(utils.requestOptionsMiddleware(z, bundle, options)).then((response) => {
                    response.throwForStatus();
                    const results = utils.responseOptionsMiddleware(z, bundle, 'updateUser', response.json);
                    return results;
                })
            },
            sample: { data: {} }
        }
    },
}
