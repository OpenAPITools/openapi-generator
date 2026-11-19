import type {
	IExecuteFunctions,
	IDataObject,
	INodeExecutionData,
	INodeType,
	INodeTypeDescription,
} from 'n8n-workflow';

import { apiRequest } from './GenericFunctions';

export class Api implements INodeType {
	description: INodeTypeDescription = {
		displayName: "OpenAPI Petstore",
		name: 'api',
		icon: 'fa:plug',
		group: ['transform'],
		version: 1,
		description: "Interact with the OpenAPI Petstore API",
		defaults: {
			name: "OpenAPI Petstore",
		},
		inputs: ['main'],
		outputs: ['main'],
		credentials: [
			{
				name: 'apiCredentialsApi',
				required: true,
			},
		],
		properties: [
			{
				displayName: 'Resource',
				name: 'resource',
				type: 'options',
				noDataExpression: true,
				options: [
					{
						name: 'Pet',
						value: 'Pet',
					},
					{
						name: 'Store',
						value: 'Store',
					},
					{
						name: 'User',
						value: 'User',
					},
				],
				default: 'Pet',
			},
			{
				displayName: 'Operation',
				name: 'operation',
				type: 'options',
				noDataExpression: true,
				displayOptions: {
					show: {
						resource: ['Pet'],
					},
				},
				options: [
					{
						name: "Add a new pet to the store",
						value: 'addPet',
						action: "Add a new pet to the store",
					},
					{
						name: "Deletes a pet",
						value: 'deletePet',
						action: "Deletes a pet",
					},
					{
						name: "Finds Pets by status",
						value: 'findPetsByStatus',
						action: "Finds Pets by status",
					},
					{
						name: "Finds Pets by tags",
						value: 'findPetsByTags',
						action: "Finds Pets by tags",
					},
					{
						name: "Find pet by ID",
						value: 'getPetById',
						action: "Find pet by ID",
					},
					{
						name: "Update an existing pet",
						value: 'updatePet',
						action: "Update an existing pet",
					},
					{
						name: "Updates a pet in the store with form data",
						value: 'updatePetWithForm',
						action: "Updates a pet in the store with form data",
					},
					{
						name: "uploads an image",
						value: 'uploadFile',
						action: "uploads an image",
					},
				],
				default: 'addPet',
			},
			{
				displayName: 'Operation',
				name: 'operation',
				type: 'options',
				noDataExpression: true,
				displayOptions: {
					show: {
						resource: ['Store'],
					},
				},
				options: [
					{
						name: "Delete purchase order by ID",
						value: 'deleteOrder',
						action: "Delete purchase order by ID",
					},
					{
						name: "Returns pet inventories by status",
						value: 'getInventory',
						action: "Returns pet inventories by status",
					},
					{
						name: "Find purchase order by ID",
						value: 'getOrderById',
						action: "Find purchase order by ID",
					},
					{
						name: "Place an order for a pet",
						value: 'placeOrder',
						action: "Place an order for a pet",
					},
				],
				default: 'deleteOrder',
			},
			{
				displayName: 'Operation',
				name: 'operation',
				type: 'options',
				noDataExpression: true,
				displayOptions: {
					show: {
						resource: ['User'],
					},
				},
				options: [
					{
						name: "Create user",
						value: 'createUser',
						action: "Create user",
					},
					{
						name: "Creates list of users with given input array",
						value: 'createUsersWithArrayInput',
						action: "Creates list of users with given input array",
					},
					{
						name: "Creates list of users with given input array",
						value: 'createUsersWithListInput',
						action: "Creates list of users with given input array",
					},
					{
						name: "Delete user",
						value: 'deleteUser',
						action: "Delete user",
					},
					{
						name: "Get user by user name",
						value: 'getUserByName',
						action: "Get user by user name",
					},
					{
						name: "Logs user into the system",
						value: 'loginUser',
						action: "Logs user into the system",
					},
					{
						name: "Logs out current logged in user session",
						value: 'logoutUser',
						action: "Logs out current logged in user session",
					},
					{
						name: "Updated user",
						value: 'updateUser',
						action: "Updated user",
					},
				],
				default: 'createUser',
			},
			{
				displayName: "Pet object that needs to be added to the store",
				name: 'Pet',
				type: 'json',
				default: {},
				required: true,
				displayOptions: {
					show: {
						operation: ['addPet'],
					},
				},
				description: "Pet object that needs to be added to the store",
			},
			{
				displayName: "Pet id to delete",
				name: 'petId',
				type: 'number',
				default: 0,
				required: true,
				displayOptions: {
					show: {
						operation: ['deletePet'],
					},
				},
				description: "Pet id to delete",
			},
			{
				displayName: "api_key",
				name: 'api_key',
				type: 'string',
				default: '',
				required: false,
				displayOptions: {
					show: {
						operation: ['deletePet'],
					},
				},
				description: "api_key",
			},
			{
				displayName: "Status values that need to be considered for filter",
				name: 'status',
				type: 'options',
				options: [
					{ name: "AVAILABLE", value: "available" },
					{ name: "PENDING", value: "pending" },
					{ name: "SOLD", value: "sold" },
				],
				default: "available",
				required: true,
				displayOptions: {
					show: {
						operation: ['findPetsByStatus'],
					},
				},
				description: "Status values that need to be considered for filter",
			},
			{
				displayName: "Tags to filter by",
				name: 'tags',
				type: 'json',
				default: {},
				required: true,
				displayOptions: {
					show: {
						operation: ['findPetsByTags'],
					},
				},
				description: "Tags to filter by",
			},
			{
				displayName: "ID of pet to return",
				name: 'petId',
				type: 'number',
				default: 0,
				required: true,
				displayOptions: {
					show: {
						operation: ['getPetById'],
					},
				},
				description: "ID of pet to return",
			},
			{
				displayName: "Pet object that needs to be added to the store",
				name: 'Pet',
				type: 'json',
				default: {},
				required: true,
				displayOptions: {
					show: {
						operation: ['updatePet'],
					},
				},
				description: "Pet object that needs to be added to the store",
			},
			{
				displayName: "ID of pet that needs to be updated",
				name: 'petId',
				type: 'number',
				default: 0,
				required: true,
				displayOptions: {
					show: {
						operation: ['updatePetWithForm'],
					},
				},
				description: "ID of pet that needs to be updated",
			},
			{
				displayName: "Updated name of the pet",
				name: 'name',
				type: 'string',
				default: '',
				required: false,
				displayOptions: {
					show: {
						operation: ['updatePetWithForm'],
					},
				},
				description: "Updated name of the pet",
			},
			{
				displayName: "Updated status of the pet",
				name: 'status',
				type: 'string',
				default: '',
				required: false,
				displayOptions: {
					show: {
						operation: ['updatePetWithForm'],
					},
				},
				description: "Updated status of the pet",
			},
			{
				displayName: "ID of pet to update",
				name: 'petId',
				type: 'number',
				default: 0,
				required: true,
				displayOptions: {
					show: {
						operation: ['uploadFile'],
					},
				},
				description: "ID of pet to update",
			},
			{
				displayName: "Additional data to pass to server",
				name: 'additionalMetadata',
				type: 'string',
				default: '',
				required: false,
				displayOptions: {
					show: {
						operation: ['uploadFile'],
					},
				},
				description: "Additional data to pass to server",
			},
			{
				displayName: "file to upload",
				name: 'file',
				type: 'json',
				default: {},
				required: false,
				displayOptions: {
					show: {
						operation: ['uploadFile'],
					},
				},
				description: "file to upload",
			},
			{
				displayName: "ID of the order that needs to be deleted",
				name: 'orderId',
				type: 'string',
				default: '',
				required: true,
				displayOptions: {
					show: {
						operation: ['deleteOrder'],
					},
				},
				description: "ID of the order that needs to be deleted",
			},
			{
				displayName: "ID of pet that needs to be fetched",
				name: 'orderId',
				type: 'number',
				default: 0,
				required: true,
				displayOptions: {
					show: {
						operation: ['getOrderById'],
					},
				},
				description: "ID of pet that needs to be fetched",
			},
			{
				displayName: "order placed for purchasing the pet",
				name: 'Order',
				type: 'json',
				default: {},
				required: true,
				displayOptions: {
					show: {
						operation: ['placeOrder'],
					},
				},
				description: "order placed for purchasing the pet",
			},
			{
				displayName: "Created user object",
				name: 'User',
				type: 'json',
				default: {},
				required: true,
				displayOptions: {
					show: {
						operation: ['createUser'],
					},
				},
				description: "Created user object",
			},
			{
				displayName: "List of user object",
				name: 'User',
				type: 'json',
				default: {},
				required: true,
				displayOptions: {
					show: {
						operation: ['createUsersWithArrayInput'],
					},
				},
				description: "List of user object",
			},
			{
				displayName: "List of user object",
				name: 'User',
				type: 'json',
				default: {},
				required: true,
				displayOptions: {
					show: {
						operation: ['createUsersWithListInput'],
					},
				},
				description: "List of user object",
			},
			{
				displayName: "The name that needs to be deleted",
				name: 'username',
				type: 'string',
				default: '',
				required: true,
				displayOptions: {
					show: {
						operation: ['deleteUser'],
					},
				},
				description: "The name that needs to be deleted",
			},
			{
				displayName: "The name that needs to be fetched. Use user1 for testing.",
				name: 'username',
				type: 'string',
				default: '',
				required: true,
				displayOptions: {
					show: {
						operation: ['getUserByName'],
					},
				},
				description: "The name that needs to be fetched. Use user1 for testing.",
			},
			{
				displayName: "The user name for login",
				name: 'username',
				type: 'string',
				default: '',
				required: true,
				displayOptions: {
					show: {
						operation: ['loginUser'],
					},
				},
				description: "The user name for login",
			},
			{
				displayName: "The password for login in clear text",
				name: 'password',
				type: 'string',
				default: '',
				required: true,
				displayOptions: {
					show: {
						operation: ['loginUser'],
					},
				},
				description: "The password for login in clear text",
			},
			{
				displayName: "name that need to be deleted",
				name: 'username',
				type: 'string',
				default: '',
				required: true,
				displayOptions: {
					show: {
						operation: ['updateUser'],
					},
				},
				description: "name that need to be deleted",
			},
			{
				displayName: "Updated user object",
				name: 'User',
				type: 'json',
				default: {},
				required: true,
				displayOptions: {
					show: {
						operation: ['updateUser'],
					},
				},
				description: "Updated user object",
			},
		],
	};

	async execute(this: IExecuteFunctions): Promise<INodeExecutionData[][]> {
		const items = this.getInputData();
		const returnData: IDataObject[] = [];
		const resource = this.getNodeParameter('resource', 0) as string;
		const operation = this.getNodeParameter('operation', 0) as string;

		for (let i = 0; i < items.length; i++) {
			let responseData: IDataObject = {};

			if (resource === 'Pet') {
				if (operation === 'addPet') {
					const qs: IDataObject = {};
					const body: IDataObject = {};
					let path = '/pet';
					Object.assign(body, this.getNodeParameter('Pet', i) as IDataObject);
					responseData = (await apiRequest.call(this, 'POST', path, body, qs)) as IDataObject;
				}
				if (operation === 'deletePet') {
					const qs: IDataObject = {};
					const body: IDataObject = {};
					let path = '/pet/{petId}';
					path = path.replace('{' + 'petId' + '}', encodeURIComponent(this.getNodeParameter('petId', i) as string));
					qs['api_key'] = this.getNodeParameter('api_key', i);
					responseData = (await apiRequest.call(this, 'DELETE', path, body, qs)) as IDataObject;
				}
				if (operation === 'findPetsByStatus') {
					const qs: IDataObject = {};
					const body: IDataObject = {};
					let path = '/pet/findByStatus';
					qs['status'] = this.getNodeParameter('status', i);
					responseData = (await apiRequest.call(this, 'GET', path, body, qs)) as IDataObject;
				}
				if (operation === 'findPetsByTags') {
					const qs: IDataObject = {};
					const body: IDataObject = {};
					let path = '/pet/findByTags';
					qs['tags'] = this.getNodeParameter('tags', i);
					responseData = (await apiRequest.call(this, 'GET', path, body, qs)) as IDataObject;
				}
				if (operation === 'getPetById') {
					const qs: IDataObject = {};
					const body: IDataObject = {};
					let path = '/pet/{petId}';
					path = path.replace('{' + 'petId' + '}', encodeURIComponent(this.getNodeParameter('petId', i) as string));
					responseData = (await apiRequest.call(this, 'GET', path, body, qs)) as IDataObject;
				}
				if (operation === 'updatePet') {
					const qs: IDataObject = {};
					const body: IDataObject = {};
					let path = '/pet';
					Object.assign(body, this.getNodeParameter('Pet', i) as IDataObject);
					responseData = (await apiRequest.call(this, 'PUT', path, body, qs)) as IDataObject;
				}
				if (operation === 'updatePetWithForm') {
					const qs: IDataObject = {};
					const body: IDataObject = {};
					let path = '/pet/{petId}';
					path = path.replace('{' + 'petId' + '}', encodeURIComponent(this.getNodeParameter('petId', i) as string));
					responseData = (await apiRequest.call(this, 'POST', path, body, qs)) as IDataObject;
				}
				if (operation === 'uploadFile') {
					const qs: IDataObject = {};
					const body: IDataObject = {};
					let path = '/pet/{petId}/uploadImage';
					path = path.replace('{' + 'petId' + '}', encodeURIComponent(this.getNodeParameter('petId', i) as string));
					responseData = (await apiRequest.call(this, 'POST', path, body, qs)) as IDataObject;
				}
			}
			if (resource === 'Store') {
				if (operation === 'deleteOrder') {
					const qs: IDataObject = {};
					const body: IDataObject = {};
					let path = '/store/order/{orderId}';
					path = path.replace('{' + 'orderId' + '}', encodeURIComponent(this.getNodeParameter('orderId', i) as string));
					responseData = (await apiRequest.call(this, 'DELETE', path, body, qs)) as IDataObject;
				}
				if (operation === 'getInventory') {
					const qs: IDataObject = {};
					const body: IDataObject = {};
					let path = '/store/inventory';
					responseData = (await apiRequest.call(this, 'GET', path, body, qs)) as IDataObject;
				}
				if (operation === 'getOrderById') {
					const qs: IDataObject = {};
					const body: IDataObject = {};
					let path = '/store/order/{orderId}';
					path = path.replace('{' + 'orderId' + '}', encodeURIComponent(this.getNodeParameter('orderId', i) as string));
					responseData = (await apiRequest.call(this, 'GET', path, body, qs)) as IDataObject;
				}
				if (operation === 'placeOrder') {
					const qs: IDataObject = {};
					const body: IDataObject = {};
					let path = '/store/order';
					Object.assign(body, this.getNodeParameter('Order', i) as IDataObject);
					responseData = (await apiRequest.call(this, 'POST', path, body, qs)) as IDataObject;
				}
			}
			if (resource === 'User') {
				if (operation === 'createUser') {
					const qs: IDataObject = {};
					const body: IDataObject = {};
					let path = '/user';
					Object.assign(body, this.getNodeParameter('User', i) as IDataObject);
					responseData = (await apiRequest.call(this, 'POST', path, body, qs)) as IDataObject;
				}
				if (operation === 'createUsersWithArrayInput') {
					const qs: IDataObject = {};
					const body: IDataObject = {};
					let path = '/user/createWithArray';
					Object.assign(body, this.getNodeParameter('User', i) as IDataObject);
					responseData = (await apiRequest.call(this, 'POST', path, body, qs)) as IDataObject;
				}
				if (operation === 'createUsersWithListInput') {
					const qs: IDataObject = {};
					const body: IDataObject = {};
					let path = '/user/createWithList';
					Object.assign(body, this.getNodeParameter('User', i) as IDataObject);
					responseData = (await apiRequest.call(this, 'POST', path, body, qs)) as IDataObject;
				}
				if (operation === 'deleteUser') {
					const qs: IDataObject = {};
					const body: IDataObject = {};
					let path = '/user/{username}';
					path = path.replace('{' + 'username' + '}', encodeURIComponent(this.getNodeParameter('username', i) as string));
					responseData = (await apiRequest.call(this, 'DELETE', path, body, qs)) as IDataObject;
				}
				if (operation === 'getUserByName') {
					const qs: IDataObject = {};
					const body: IDataObject = {};
					let path = '/user/{username}';
					path = path.replace('{' + 'username' + '}', encodeURIComponent(this.getNodeParameter('username', i) as string));
					responseData = (await apiRequest.call(this, 'GET', path, body, qs)) as IDataObject;
				}
				if (operation === 'loginUser') {
					const qs: IDataObject = {};
					const body: IDataObject = {};
					let path = '/user/login';
					qs['username'] = this.getNodeParameter('username', i);
					qs['password'] = this.getNodeParameter('password', i);
					responseData = (await apiRequest.call(this, 'GET', path, body, qs)) as IDataObject;
				}
				if (operation === 'logoutUser') {
					const qs: IDataObject = {};
					const body: IDataObject = {};
					let path = '/user/logout';
					responseData = (await apiRequest.call(this, 'GET', path, body, qs)) as IDataObject;
				}
				if (operation === 'updateUser') {
					const qs: IDataObject = {};
					const body: IDataObject = {};
					let path = '/user/{username}';
					path = path.replace('{' + 'username' + '}', encodeURIComponent(this.getNodeParameter('username', i) as string));
					Object.assign(body, this.getNodeParameter('User', i) as IDataObject);
					responseData = (await apiRequest.call(this, 'PUT', path, body, qs)) as IDataObject;
				}
			}

			returnData.push(responseData);
		}

		return [this.helpers.returnJsonArray(returnData)];
	}
}
