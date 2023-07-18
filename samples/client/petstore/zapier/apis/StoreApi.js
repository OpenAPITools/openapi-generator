const samples = require('../samples/StoreApi');
const Order = require('../models/Order');
const utils = require('../utils/utils');

module.exports = {
    deleteOrder: {
        key: 'deleteOrder',
        noun: 'store',
        display: {
            label: 'Delete purchase order by ID',
            description: 'For valid response try integer IDs with value &lt; 1000. Anything above 1000 or nonintegers will generate API errors',
            hidden: false,
        },
        operation: {
            inputFields: [
                {
                    key: 'orderId',
                    label: 'ID of the order that needs to be deleted',
                    type: 'string',
                    required: true,
                },
            ],
            outputFields: [
            ],
            perform: async (z, bundle) => {
                const options = {
                    url: utils.replacePathParameters('http://petstore.swagger.io/v2/store/order/{orderId}'),
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
    getInventory: {
        key: 'getInventory',
        noun: 'store',
        display: {
            label: 'Returns pet inventories by status',
            description: 'Returns a map of status codes to quantities',
            hidden: false,
        },
        operation: {
            inputFields: [
            ],
            outputFields: [
            ],
            perform: async (z, bundle) => {
                const options = {
                    url: utils.replacePathParameters('http://petstore.swagger.io/v2/store/inventory'),
                    method: 'GET',
                    removeMissingValuesFrom: { params: true, body: true },
                    headers: {
                        'Authorization': 'Bearer {{bundle.authData.access_token}}',
                        'Content-Type': '',
                        'Accept': 'application/json',
                    },
                    params: {
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
    getOrderById: {
        key: 'getOrderById',
        noun: 'store',
        display: {
            label: 'Find purchase order by ID',
            description: 'For valid response try integer IDs with value &lt;&#x3D; 5 or &gt; 10. Other values will generate exceptions',
            hidden: false,
        },
        operation: {
            inputFields: [
                {
                    key: 'orderId',
                    label: 'ID of pet that needs to be fetched',
                    type: 'number',
                    required: true,
                },
            ],
            outputFields: [
                ...Order.fields('', false),
            ],
            perform: async (z, bundle) => {
                const options = {
                    url: utils.replacePathParameters('http://petstore.swagger.io/v2/store/order/{orderId}'),
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
            sample: samples['OrderSample']
        }
    },
    placeOrder: {
        key: 'placeOrder',
        noun: 'store',
        display: {
            label: 'Place an order for a pet',
            description: '',
            hidden: false,
        },
        operation: {
            inputFields: [
                ...Order.fields(),
            ],
            outputFields: [
                ...Order.fields('', false),
            ],
            perform: async (z, bundle) => {
                const options = {
                    url: utils.replacePathParameters('http://petstore.swagger.io/v2/store/order'),
                    method: 'POST',
                    removeMissingValuesFrom: { params: true, body: true },
                    headers: {
                        'Authorization': 'Bearer {{bundle.authData.access_token}}',
                        'Content-Type': 'application/json',
                        'Accept': 'application/xml, application/json',
                    },
                    params: {
                    },
                    body: {
                        ...Order.mapping(bundle),
                    },
                }
                return z.request(options).then((response) => {
                    response.throwForStatus();
                    const results = response.json;
                    return results;
                })
            },
            sample: samples['OrderSample']
        }
    },
}
