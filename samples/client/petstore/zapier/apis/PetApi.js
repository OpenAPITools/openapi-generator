const samples = require('../samples/PetApi');
const ApiResponse = require('../models/ApiResponse');
const Pet = require('../models/Pet');
const utils = require('../utils/utils');
const FormData = require('form-data');

module.exports = {
    addPet: {
        key: 'addPet',
        noun: 'pet',
        display: {
            label: 'Add a new pet to the store',
            description: '',
            hidden: false,
        },
        operation: {
            inputFields: [
                ...Pet.fields(),
            ],
            outputFields: [
                ...Pet.fields('', false),
            ],
            perform: async (z, bundle) => {
                const options = {
                    url: utils.replacePathParameters('http://petstore.swagger.io/v2/pet'),
                    method: 'POST',
                    removeMissingValuesFrom: { params: true, body: true },
                    headers: {
                        'Authorization': 'Bearer {{bundle.authData.access_token}}',
                        'Content-Type': 'application/json, application/xml',
                        'Accept': 'application/xml, application/json',
                    },
                    params: {
                    },
                    body: {
                        ...Pet.mapping(bundle),
                    },
                }
                return z.request(options).then((response) => {
                    response.throwForStatus();
                    const results = response.json;
                    return results;
                })
            },
            sample: samples['PetSample']
        }
    },
    deletePet: {
        key: 'deletePet',
        noun: 'pet',
        display: {
            label: 'Deletes a pet',
            description: '',
            hidden: false,
        },
        operation: {
            inputFields: [
                {
                    key: 'petId',
                    label: 'Pet id to delete',
                    type: 'number',
                    required: true,
                },
                {
                    key: 'api_key',
                    label: '',
                    type: 'string',
                },
            ],
            outputFields: [
            ],
            perform: async (z, bundle) => {
                const options = {
                    url: utils.replacePathParameters('http://petstore.swagger.io/v2/pet/{petId}'),
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
    findPetsByStatus: {
        key: 'findPetsByStatus',
        noun: 'pet',
        display: {
            label: 'Finds Pets by status',
            description: 'Multiple status values can be provided with comma separated strings',
            hidden: false,
        },
        operation: {
            inputFields: [
                {
                    key: 'status',
                    label: 'Status values that need to be considered for filter',
                    type: 'string',
                }
            ],
            outputFields: [
            ],
            perform: async (z, bundle) => {
                const options = {
                    url: utils.replacePathParameters('http://petstore.swagger.io/v2/pet/findByStatus'),
                    method: 'GET',
                    removeMissingValuesFrom: { params: true, body: true },
                    headers: {
                        'Authorization': 'Bearer {{bundle.authData.access_token}}',
                        'Content-Type': '',
                        'Accept': 'application/xml, application/json',
                    },
                    params: {
                        'status': bundle.inputData?.['status'],
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
            sample: samples['PetSample']
        }
    },
    findPetsByTags: {
        key: 'findPetsByTags',
        noun: 'pet',
        display: {
            label: 'Finds Pets by tags',
            description: 'Multiple tags can be provided with comma separated strings. Use tag1, tag2, tag3 for testing.',
            hidden: false,
        },
        operation: {
            inputFields: [
                {
                    key: 'tags',
                    label: 'Tags to filter by',
                    type: 'string',
                }
            ],
            outputFields: [
            ],
            perform: async (z, bundle) => {
                const options = {
                    url: utils.replacePathParameters('http://petstore.swagger.io/v2/pet/findByTags'),
                    method: 'GET',
                    removeMissingValuesFrom: { params: true, body: true },
                    headers: {
                        'Authorization': 'Bearer {{bundle.authData.access_token}}',
                        'Content-Type': '',
                        'Accept': 'application/xml, application/json',
                    },
                    params: {
                        'tags': bundle.inputData?.['tags'],
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
            sample: samples['PetSample']
        }
    },
    getPetById: {
        key: 'getPetById',
        noun: 'pet',
        display: {
            label: 'Find pet by ID',
            description: 'Returns a single pet',
            hidden: false,
        },
        operation: {
            inputFields: [
                {
                    key: 'petId',
                    label: 'ID of pet to return',
                    type: 'number',
                    required: true,
                },
            ],
            outputFields: [
                ...Pet.fields('', false),
            ],
            perform: async (z, bundle) => {
                const options = {
                    url: utils.replacePathParameters('http://petstore.swagger.io/v2/pet/{petId}'),
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
            sample: samples['PetSample']
        }
    },
    updatePet: {
        key: 'updatePet',
        noun: 'pet',
        display: {
            label: 'Update an existing pet',
            description: '',
            hidden: false,
        },
        operation: {
            inputFields: [
                ...Pet.fields(),
            ],
            outputFields: [
                ...Pet.fields('', false),
            ],
            perform: async (z, bundle) => {
                const options = {
                    url: utils.replacePathParameters('http://petstore.swagger.io/v2/pet'),
                    method: 'PUT',
                    removeMissingValuesFrom: { params: true, body: true },
                    headers: {
                        'Authorization': 'Bearer {{bundle.authData.access_token}}',
                        'Content-Type': 'application/json, application/xml',
                        'Accept': 'application/xml, application/json',
                    },
                    params: {
                    },
                    body: {
                        ...Pet.mapping(bundle),
                    },
                }
                return z.request(options).then((response) => {
                    response.throwForStatus();
                    const results = response.json;
                    return results;
                })
            },
            sample: samples['PetSample']
        }
    },
    updatePetWithForm: {
        key: 'updatePetWithForm',
        noun: 'pet',
        display: {
            label: 'Updates a pet in the store with form data',
            description: '',
            hidden: false,
        },
        operation: {
            inputFields: [
                {
                    key: 'petId',
                    label: 'ID of pet that needs to be updated',
                    type: 'number',
                    required: true,
                },
                {
                    key: 'name',
                    label: 'Updated name of the pet',
                    type: 'string',
                },
                {
                    key: 'status',
                    label: 'Updated status of the pet',
                    type: 'string',
                },
            ],
            outputFields: [
            ],
            perform: async (z, bundle) => {
                const options = {
                    url: utils.replacePathParameters('http://petstore.swagger.io/v2/pet/{petId}'),
                    method: 'POST',
                    removeMissingValuesFrom: { params: true, body: true },
                    headers: {
                        'Authorization': 'Bearer {{bundle.authData.access_token}}',
                        'Content-Type': 'application/x-www-form-urlencoded',
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
    uploadFile: {
        key: 'uploadFile',
        noun: 'pet',
        display: {
            label: 'uploads an image',
            description: '',
            hidden: false,
        },
        operation: {
            inputFields: [
                {
                    key: 'petId',
                    label: 'ID of pet to update',
                    type: 'number',
                    required: true,
                },
                {
                    key: 'additionalMetadata',
                    label: 'Additional data to pass to server',
                    type: 'string',
                },
                {
                    key: 'file',
                    label: 'file to upload',
                    type: 'file',
                },
            ],
            outputFields: [
                ...ApiResponse.fields('', false),
            ],
            perform: async (z, bundle) => {
                const formData = new FormData()
                formData.append('additionalMetadata', bundle.inputData?.['additionalMetadata'])
                const filename = bundle.inputData?.['filename'] || bundle.inputData?.['file'].split('/').slice(-1)[0]
                formData.append('file', (await (await z.request({url: bundle.inputData?.['file'], method: 'GET', raw: true})).buffer()), { filename: filename })
                const options = {
                    url: utils.replacePathParameters('http://petstore.swagger.io/v2/pet/{petId}/uploadImage'),
                    method: 'POST',
                    removeMissingValuesFrom: { params: true, body: true },
                    headers: {
                        'Authorization': 'Bearer {{bundle.authData.access_token}}',
                        
                        'Accept': 'application/json',
                    },
                    params: {
                    },
                    body: formData,
                }
                return z.request(options).then((response) => {
                    response.throwForStatus();
                    const results = response.json;
                    return results;
                })
            },
            sample: samples['ApiResponseSample']
        }
    },
}
