import type {
	IExecuteFunctions,
	IDataObject,
	INodeExecutionData,
	INodeType,
	INodeTypeDescription,
} from 'n8n-workflow';
import { NodeOperationError } from 'n8n-workflow';

import { apiRequest } from './GenericFunctions';

export class N8nNodesOpenapiPetstoreApi implements INodeType {
	description: INodeTypeDescription = {
		displayName: "OpenAPI Petstore",
		name: 'n8nNodesOpenapiPetstoreApi',
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
				name: 'n8nNodesOpenapiPetstoreApiKeyCredentials',
				required: false,
			},
			{
				name: 'n8nNodesOpenapiPetstorePetstoreAuthCredentials',
				required: false,
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
				displayName: "id",
				name: 'id',
				type: 'number',
				default: 0,
				required: false,
				displayOptions: {
					show: {
						operation: ['addPet'],
					},
				},
				description: "id",
			},
			{
				displayName: "category",
				name: 'category',
				type: 'json',
				default: {},
				required: false,
				displayOptions: {
					show: {
						operation: ['addPet'],
					},
				},
				description: "category",
			},
			{
				displayName: "name",
				name: 'name',
				type: 'string',
				default: '',
				required: true,
				displayOptions: {
					show: {
						operation: ['addPet'],
					},
				},
				description: "name",
			},
			{
				displayName: "photoUrls",
				name: 'photoUrls',
				type: 'collection',
				typeOptions: {
					multipleValues: true,
				},
				options: [
					{
						displayName: 'Value',
						name: 'value',
						type: 'string',
						default: '',
					},
				],
				default: [],
				required: true,
				displayOptions: {
					show: {
						operation: ['addPet'],
					},
				},
				description: "photoUrls",
			},
			{
				displayName: "tags",
				name: 'tags',
				type: 'json',
				default: [],
				required: false,
				displayOptions: {
					show: {
						operation: ['addPet'],
					},
				},
				description: "tags",
			},
			{
				displayName: "pet status in the store",
				name: 'status',
				type: 'options',
				options: [
					{ name: "AVAILABLE", value: "available" },
					{ name: "PENDING", value: "pending" },
					{ name: "SOLD", value: "sold" },
				],
				default: "available",
				required: false,
				displayOptions: {
					show: {
						operation: ['addPet'],
					},
				},
				description: "pet status in the store",
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
				type: 'collection',
				typeOptions: {
					multipleValues: true,
				},
				options: [
					{
						displayName: 'Value',
						name: 'value',
						type: 'options',
						options: [
							{ name: "AVAILABLE", value: "available" },
							{ name: "PENDING", value: "pending" },
							{ name: "SOLD", value: "sold" },
						],
						default: "available",
					},
				],
				default: [],
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
				type: 'collection',
				typeOptions: {
					multipleValues: true,
				},
				options: [
					{
						displayName: 'Value',
						name: 'value',
						type: 'string',
						default: '',
					},
				],
				default: [],
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
				displayName: "id",
				name: 'id',
				type: 'number',
				default: 0,
				required: false,
				displayOptions: {
					show: {
						operation: ['updatePet'],
					},
				},
				description: "id",
			},
			{
				displayName: "category",
				name: 'category',
				type: 'json',
				default: {},
				required: false,
				displayOptions: {
					show: {
						operation: ['updatePet'],
					},
				},
				description: "category",
			},
			{
				displayName: "name",
				name: 'name',
				type: 'string',
				default: '',
				required: true,
				displayOptions: {
					show: {
						operation: ['updatePet'],
					},
				},
				description: "name",
			},
			{
				displayName: "photoUrls",
				name: 'photoUrls',
				type: 'collection',
				typeOptions: {
					multipleValues: true,
				},
				options: [
					{
						displayName: 'Value',
						name: 'value',
						type: 'string',
						default: '',
					},
				],
				default: [],
				required: true,
				displayOptions: {
					show: {
						operation: ['updatePet'],
					},
				},
				description: "photoUrls",
			},
			{
				displayName: "tags",
				name: 'tags',
				type: 'json',
				default: [],
				required: false,
				displayOptions: {
					show: {
						operation: ['updatePet'],
					},
				},
				description: "tags",
			},
			{
				displayName: "pet status in the store",
				name: 'status',
				type: 'options',
				options: [
					{ name: "AVAILABLE", value: "available" },
					{ name: "PENDING", value: "pending" },
					{ name: "SOLD", value: "sold" },
				],
				default: "available",
				required: false,
				displayOptions: {
					show: {
						operation: ['updatePet'],
					},
				},
				description: "pet status in the store",
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
				displayName: "id",
				name: 'id',
				type: 'number',
				default: 0,
				required: false,
				displayOptions: {
					show: {
						operation: ['placeOrder'],
					},
				},
				description: "id",
			},
			{
				displayName: "petId",
				name: 'petId',
				type: 'number',
				default: 0,
				required: false,
				displayOptions: {
					show: {
						operation: ['placeOrder'],
					},
				},
				description: "petId",
			},
			{
				displayName: "quantity",
				name: 'quantity',
				type: 'number',
				default: 0,
				required: false,
				displayOptions: {
					show: {
						operation: ['placeOrder'],
					},
				},
				description: "quantity",
			},
			{
				displayName: "shipDate",
				name: 'shipDate',
				type: 'string',
				default: '',
				required: false,
				displayOptions: {
					show: {
						operation: ['placeOrder'],
					},
				},
				description: "shipDate",
			},
			{
				displayName: "Order Status",
				name: 'status',
				type: 'options',
				options: [
					{ name: "PLACED", value: "placed" },
					{ name: "APPROVED", value: "approved" },
					{ name: "DELIVERED", value: "delivered" },
				],
				default: "placed",
				required: false,
				displayOptions: {
					show: {
						operation: ['placeOrder'],
					},
				},
				description: "Order Status",
			},
			{
				displayName: "complete",
				name: 'complete',
				type: 'boolean',
				default: false,
				required: false,
				displayOptions: {
					show: {
						operation: ['placeOrder'],
					},
				},
				description: "complete",
			},
			{
				displayName: "id",
				name: 'id',
				type: 'number',
				default: 0,
				required: false,
				displayOptions: {
					show: {
						operation: ['createUser'],
					},
				},
				description: "id",
			},
			{
				displayName: "username",
				name: 'username',
				type: 'string',
				default: '',
				required: false,
				displayOptions: {
					show: {
						operation: ['createUser'],
					},
				},
				description: "username",
			},
			{
				displayName: "firstName",
				name: 'firstName',
				type: 'string',
				default: '',
				required: false,
				displayOptions: {
					show: {
						operation: ['createUser'],
					},
				},
				description: "firstName",
			},
			{
				displayName: "lastName",
				name: 'lastName',
				type: 'string',
				default: '',
				required: false,
				displayOptions: {
					show: {
						operation: ['createUser'],
					},
				},
				description: "lastName",
			},
			{
				displayName: "email",
				name: 'email',
				type: 'string',
				default: '',
				required: false,
				displayOptions: {
					show: {
						operation: ['createUser'],
					},
				},
				description: "email",
			},
			{
				displayName: "password",
				name: 'password',
				type: 'string',
				default: '',
				required: false,
				displayOptions: {
					show: {
						operation: ['createUser'],
					},
				},
				description: "password",
			},
			{
				displayName: "phone",
				name: 'phone',
				type: 'string',
				default: '',
				required: false,
				displayOptions: {
					show: {
						operation: ['createUser'],
					},
				},
				description: "phone",
			},
			{
				displayName: "User Status",
				name: 'userStatus',
				type: 'number',
				default: 0,
				required: false,
				displayOptions: {
					show: {
						operation: ['createUser'],
					},
				},
				description: "User Status",
			},
			{
				displayName: "List of user object",
				name: 'User',
				type: 'json',
				default: [],
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
				default: [],
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
				displayName: "id",
				name: 'id',
				type: 'number',
				default: 0,
				required: false,
				displayOptions: {
					show: {
						operation: ['updateUser'],
					},
				},
				description: "id",
			},
			{
				displayName: "username (request body)",
				name: 'username_f02988d6',
				type: 'string',
				default: '',
				required: false,
				displayOptions: {
					show: {
						operation: ['updateUser'],
					},
				},
				description: "username (request body)",
			},
			{
				displayName: "firstName",
				name: 'firstName',
				type: 'string',
				default: '',
				required: false,
				displayOptions: {
					show: {
						operation: ['updateUser'],
					},
				},
				description: "firstName",
			},
			{
				displayName: "lastName",
				name: 'lastName',
				type: 'string',
				default: '',
				required: false,
				displayOptions: {
					show: {
						operation: ['updateUser'],
					},
				},
				description: "lastName",
			},
			{
				displayName: "email",
				name: 'email',
				type: 'string',
				default: '',
				required: false,
				displayOptions: {
					show: {
						operation: ['updateUser'],
					},
				},
				description: "email",
			},
			{
				displayName: "password",
				name: 'password',
				type: 'string',
				default: '',
				required: false,
				displayOptions: {
					show: {
						operation: ['updateUser'],
					},
				},
				description: "password",
			},
			{
				displayName: "phone",
				name: 'phone',
				type: 'string',
				default: '',
				required: false,
				displayOptions: {
					show: {
						operation: ['updateUser'],
					},
				},
				description: "phone",
			},
			{
				displayName: "User Status",
				name: 'userStatus',
				type: 'number',
				default: 0,
				required: false,
				displayOptions: {
					show: {
						operation: ['updateUser'],
					},
				},
				description: "User Status",
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
			let matched = false;

			if (resource === 'Pet') {
				if (operation === 'addPet') {
					matched = true;
					const qs: IDataObject = {};
					const headers: IDataObject = {};
					const cookieParts: string[] = [];
					const formFields: IDataObject = {};
					const body: IDataObject = {};
					let wholeBodyArray: unknown[] | undefined;
					let wholeBodyScalar: string | number | boolean | undefined;
					let path = '/pet';
					body['id'] = this.getNodeParameter('id', i);
					body['category'] = this.getNodeParameter('category', i);
					body['name'] = this.getNodeParameter('name', i);
					body['photoUrls'] = (this.getNodeParameter('photoUrls', i) as IDataObject[]).map((entry) => entry.value);
					body['tags'] = this.getNodeParameter('tags', i);
					body['status'] = this.getNodeParameter('status', i);
					if (cookieParts.length > 0) {
						headers['Cookie'] = cookieParts.join('; ');
					}
					responseData = (await apiRequest.call(this, 'POST', path, wholeBodyArray ?? wholeBodyScalar ?? body, qs, headers, formFields, 'n8nNodesOpenapiPetstorePetstoreAuthCredentials')) as IDataObject;
				}
				if (operation === 'deletePet') {
					matched = true;
					const qs: IDataObject = {};
					const headers: IDataObject = {};
					const cookieParts: string[] = [];
					const formFields: IDataObject = {};
					const body: IDataObject = {};
					let wholeBodyArray: unknown[] | undefined;
					let wholeBodyScalar: string | number | boolean | undefined;
					let path = '/pet/{petId}';
					path = path.replace('{' + 'petId' + '}', encodeURIComponent(this.getNodeParameter('petId', i) as string));
					headers['api_key'] = this.getNodeParameter('api_key', i);
					if (cookieParts.length > 0) {
						headers['Cookie'] = cookieParts.join('; ');
					}
					responseData = (await apiRequest.call(this, 'DELETE', path, wholeBodyArray ?? wholeBodyScalar ?? body, qs, headers, formFields, 'n8nNodesOpenapiPetstorePetstoreAuthCredentials')) as IDataObject;
				}
				if (operation === 'findPetsByStatus') {
					matched = true;
					const qs: IDataObject = {};
					const headers: IDataObject = {};
					const cookieParts: string[] = [];
					const formFields: IDataObject = {};
					const body: IDataObject = {};
					let wholeBodyArray: unknown[] | undefined;
					let wholeBodyScalar: string | number | boolean | undefined;
					let path = '/pet/findByStatus';
					qs['status'] = (this.getNodeParameter('status', i) as IDataObject[]).map((entry) => entry.value);
					if (cookieParts.length > 0) {
						headers['Cookie'] = cookieParts.join('; ');
					}
					responseData = (await apiRequest.call(this, 'GET', path, wholeBodyArray ?? wholeBodyScalar ?? body, qs, headers, formFields, 'n8nNodesOpenapiPetstorePetstoreAuthCredentials')) as IDataObject;
				}
				if (operation === 'findPetsByTags') {
					matched = true;
					const qs: IDataObject = {};
					const headers: IDataObject = {};
					const cookieParts: string[] = [];
					const formFields: IDataObject = {};
					const body: IDataObject = {};
					let wholeBodyArray: unknown[] | undefined;
					let wholeBodyScalar: string | number | boolean | undefined;
					let path = '/pet/findByTags';
					qs['tags'] = (this.getNodeParameter('tags', i) as IDataObject[]).map((entry) => entry.value);
					if (cookieParts.length > 0) {
						headers['Cookie'] = cookieParts.join('; ');
					}
					responseData = (await apiRequest.call(this, 'GET', path, wholeBodyArray ?? wholeBodyScalar ?? body, qs, headers, formFields, 'n8nNodesOpenapiPetstorePetstoreAuthCredentials')) as IDataObject;
				}
				if (operation === 'getPetById') {
					matched = true;
					const qs: IDataObject = {};
					const headers: IDataObject = {};
					const cookieParts: string[] = [];
					const formFields: IDataObject = {};
					const body: IDataObject = {};
					let wholeBodyArray: unknown[] | undefined;
					let wholeBodyScalar: string | number | boolean | undefined;
					let path = '/pet/{petId}';
					path = path.replace('{' + 'petId' + '}', encodeURIComponent(this.getNodeParameter('petId', i) as string));
					if (cookieParts.length > 0) {
						headers['Cookie'] = cookieParts.join('; ');
					}
					responseData = (await apiRequest.call(this, 'GET', path, wholeBodyArray ?? wholeBodyScalar ?? body, qs, headers, formFields, 'n8nNodesOpenapiPetstoreApiKeyCredentials')) as IDataObject;
				}
				if (operation === 'updatePet') {
					matched = true;
					const qs: IDataObject = {};
					const headers: IDataObject = {};
					const cookieParts: string[] = [];
					const formFields: IDataObject = {};
					const body: IDataObject = {};
					let wholeBodyArray: unknown[] | undefined;
					let wholeBodyScalar: string | number | boolean | undefined;
					let path = '/pet';
					body['id'] = this.getNodeParameter('id', i);
					body['category'] = this.getNodeParameter('category', i);
					body['name'] = this.getNodeParameter('name', i);
					body['photoUrls'] = (this.getNodeParameter('photoUrls', i) as IDataObject[]).map((entry) => entry.value);
					body['tags'] = this.getNodeParameter('tags', i);
					body['status'] = this.getNodeParameter('status', i);
					if (cookieParts.length > 0) {
						headers['Cookie'] = cookieParts.join('; ');
					}
					responseData = (await apiRequest.call(this, 'PUT', path, wholeBodyArray ?? wholeBodyScalar ?? body, qs, headers, formFields, 'n8nNodesOpenapiPetstorePetstoreAuthCredentials')) as IDataObject;
				}
				if (operation === 'updatePetWithForm') {
					matched = true;
					const qs: IDataObject = {};
					const headers: IDataObject = {};
					const cookieParts: string[] = [];
					const formFields: IDataObject = {};
					const body: IDataObject = {};
					let wholeBodyArray: unknown[] | undefined;
					let wholeBodyScalar: string | number | boolean | undefined;
					let path = '/pet/{petId}';
					path = path.replace('{' + 'petId' + '}', encodeURIComponent(this.getNodeParameter('petId', i) as string));
					{
						const nameValue = this.getNodeParameter('name', i);
						if (nameValue !== '') {
							formFields['name'] = nameValue;
						}
					}
					{
						const statusValue = this.getNodeParameter('status', i);
						if (statusValue !== '') {
							formFields['status'] = statusValue;
						}
					}
					if (cookieParts.length > 0) {
						headers['Cookie'] = cookieParts.join('; ');
					}
					responseData = (await apiRequest.call(this, 'POST', path, wholeBodyArray ?? wholeBodyScalar ?? body, qs, headers, formFields, 'n8nNodesOpenapiPetstorePetstoreAuthCredentials')) as IDataObject;
				}
				if (operation === 'uploadFile') {
					matched = true;
					const qs: IDataObject = {};
					const headers: IDataObject = {};
					const cookieParts: string[] = [];
					const formFields: IDataObject = {};
					const body: IDataObject = {};
					let wholeBodyArray: unknown[] | undefined;
					let wholeBodyScalar: string | number | boolean | undefined;
					let path = '/pet/{petId}/uploadImage';
					path = path.replace('{' + 'petId' + '}', encodeURIComponent(this.getNodeParameter('petId', i) as string));
					{
						const additionalMetadataValue = this.getNodeParameter('additionalMetadata', i);
						if (additionalMetadataValue !== '') {
							formFields['additionalMetadata'] = additionalMetadataValue;
						}
					}
					// 'file' is a binary file form field; this generator does not support uploading binary data, so it is omitted from the request.
					if (cookieParts.length > 0) {
						headers['Cookie'] = cookieParts.join('; ');
					}
					responseData = (await apiRequest.call(this, 'POST', path, wholeBodyArray ?? wholeBodyScalar ?? body, qs, headers, formFields, 'n8nNodesOpenapiPetstorePetstoreAuthCredentials')) as IDataObject;
				}
			}
			if (resource === 'Store') {
				if (operation === 'deleteOrder') {
					matched = true;
					const qs: IDataObject = {};
					const headers: IDataObject = {};
					const cookieParts: string[] = [];
					const formFields: IDataObject = {};
					const body: IDataObject = {};
					let wholeBodyArray: unknown[] | undefined;
					let wholeBodyScalar: string | number | boolean | undefined;
					let path = '/store/order/{orderId}';
					path = path.replace('{' + 'orderId' + '}', encodeURIComponent(this.getNodeParameter('orderId', i) as string));
					if (cookieParts.length > 0) {
						headers['Cookie'] = cookieParts.join('; ');
					}
					responseData = (await apiRequest.call(this, 'DELETE', path, wholeBodyArray ?? wholeBodyScalar ?? body, qs, headers, formFields, '')) as IDataObject;
				}
				if (operation === 'getInventory') {
					matched = true;
					const qs: IDataObject = {};
					const headers: IDataObject = {};
					const cookieParts: string[] = [];
					const formFields: IDataObject = {};
					const body: IDataObject = {};
					let wholeBodyArray: unknown[] | undefined;
					let wholeBodyScalar: string | number | boolean | undefined;
					let path = '/store/inventory';
					if (cookieParts.length > 0) {
						headers['Cookie'] = cookieParts.join('; ');
					}
					responseData = (await apiRequest.call(this, 'GET', path, wholeBodyArray ?? wholeBodyScalar ?? body, qs, headers, formFields, 'n8nNodesOpenapiPetstoreApiKeyCredentials')) as IDataObject;
				}
				if (operation === 'getOrderById') {
					matched = true;
					const qs: IDataObject = {};
					const headers: IDataObject = {};
					const cookieParts: string[] = [];
					const formFields: IDataObject = {};
					const body: IDataObject = {};
					let wholeBodyArray: unknown[] | undefined;
					let wholeBodyScalar: string | number | boolean | undefined;
					let path = '/store/order/{orderId}';
					path = path.replace('{' + 'orderId' + '}', encodeURIComponent(this.getNodeParameter('orderId', i) as string));
					if (cookieParts.length > 0) {
						headers['Cookie'] = cookieParts.join('; ');
					}
					responseData = (await apiRequest.call(this, 'GET', path, wholeBodyArray ?? wholeBodyScalar ?? body, qs, headers, formFields, '')) as IDataObject;
				}
				if (operation === 'placeOrder') {
					matched = true;
					const qs: IDataObject = {};
					const headers: IDataObject = {};
					const cookieParts: string[] = [];
					const formFields: IDataObject = {};
					const body: IDataObject = {};
					let wholeBodyArray: unknown[] | undefined;
					let wholeBodyScalar: string | number | boolean | undefined;
					let path = '/store/order';
					body['id'] = this.getNodeParameter('id', i);
					body['petId'] = this.getNodeParameter('petId', i);
					body['quantity'] = this.getNodeParameter('quantity', i);
					body['shipDate'] = this.getNodeParameter('shipDate', i);
					body['status'] = this.getNodeParameter('status', i);
					body['complete'] = this.getNodeParameter('complete', i);
					if (cookieParts.length > 0) {
						headers['Cookie'] = cookieParts.join('; ');
					}
					responseData = (await apiRequest.call(this, 'POST', path, wholeBodyArray ?? wholeBodyScalar ?? body, qs, headers, formFields, '')) as IDataObject;
				}
			}
			if (resource === 'User') {
				if (operation === 'createUser') {
					matched = true;
					const qs: IDataObject = {};
					const headers: IDataObject = {};
					const cookieParts: string[] = [];
					const formFields: IDataObject = {};
					const body: IDataObject = {};
					let wholeBodyArray: unknown[] | undefined;
					let wholeBodyScalar: string | number | boolean | undefined;
					let path = '/user';
					body['id'] = this.getNodeParameter('id', i);
					body['username'] = this.getNodeParameter('username', i);
					body['firstName'] = this.getNodeParameter('firstName', i);
					body['lastName'] = this.getNodeParameter('lastName', i);
					body['email'] = this.getNodeParameter('email', i);
					body['password'] = this.getNodeParameter('password', i);
					body['phone'] = this.getNodeParameter('phone', i);
					body['userStatus'] = this.getNodeParameter('userStatus', i);
					if (cookieParts.length > 0) {
						headers['Cookie'] = cookieParts.join('; ');
					}
					responseData = (await apiRequest.call(this, 'POST', path, wholeBodyArray ?? wholeBodyScalar ?? body, qs, headers, formFields, 'n8nNodesOpenapiPetstoreApiKeyCredentials')) as IDataObject;
				}
				if (operation === 'createUsersWithArrayInput') {
					matched = true;
					const qs: IDataObject = {};
					const headers: IDataObject = {};
					const cookieParts: string[] = [];
					const formFields: IDataObject = {};
					const body: IDataObject = {};
					let wholeBodyArray: unknown[] | undefined;
					let wholeBodyScalar: string | number | boolean | undefined;
					let path = '/user/createWithArray';
					wholeBodyArray = this.getNodeParameter('User', i) as unknown[];
					if (cookieParts.length > 0) {
						headers['Cookie'] = cookieParts.join('; ');
					}
					responseData = (await apiRequest.call(this, 'POST', path, wholeBodyArray ?? wholeBodyScalar ?? body, qs, headers, formFields, 'n8nNodesOpenapiPetstoreApiKeyCredentials')) as IDataObject;
				}
				if (operation === 'createUsersWithListInput') {
					matched = true;
					const qs: IDataObject = {};
					const headers: IDataObject = {};
					const cookieParts: string[] = [];
					const formFields: IDataObject = {};
					const body: IDataObject = {};
					let wholeBodyArray: unknown[] | undefined;
					let wholeBodyScalar: string | number | boolean | undefined;
					let path = '/user/createWithList';
					wholeBodyArray = this.getNodeParameter('User', i) as unknown[];
					if (cookieParts.length > 0) {
						headers['Cookie'] = cookieParts.join('; ');
					}
					responseData = (await apiRequest.call(this, 'POST', path, wholeBodyArray ?? wholeBodyScalar ?? body, qs, headers, formFields, 'n8nNodesOpenapiPetstoreApiKeyCredentials')) as IDataObject;
				}
				if (operation === 'deleteUser') {
					matched = true;
					const qs: IDataObject = {};
					const headers: IDataObject = {};
					const cookieParts: string[] = [];
					const formFields: IDataObject = {};
					const body: IDataObject = {};
					let wholeBodyArray: unknown[] | undefined;
					let wholeBodyScalar: string | number | boolean | undefined;
					let path = '/user/{username}';
					path = path.replace('{' + 'username' + '}', encodeURIComponent(this.getNodeParameter('username', i) as string));
					if (cookieParts.length > 0) {
						headers['Cookie'] = cookieParts.join('; ');
					}
					responseData = (await apiRequest.call(this, 'DELETE', path, wholeBodyArray ?? wholeBodyScalar ?? body, qs, headers, formFields, 'n8nNodesOpenapiPetstoreApiKeyCredentials')) as IDataObject;
				}
				if (operation === 'getUserByName') {
					matched = true;
					const qs: IDataObject = {};
					const headers: IDataObject = {};
					const cookieParts: string[] = [];
					const formFields: IDataObject = {};
					const body: IDataObject = {};
					let wholeBodyArray: unknown[] | undefined;
					let wholeBodyScalar: string | number | boolean | undefined;
					let path = '/user/{username}';
					path = path.replace('{' + 'username' + '}', encodeURIComponent(this.getNodeParameter('username', i) as string));
					if (cookieParts.length > 0) {
						headers['Cookie'] = cookieParts.join('; ');
					}
					responseData = (await apiRequest.call(this, 'GET', path, wholeBodyArray ?? wholeBodyScalar ?? body, qs, headers, formFields, '')) as IDataObject;
				}
				if (operation === 'loginUser') {
					matched = true;
					const qs: IDataObject = {};
					const headers: IDataObject = {};
					const cookieParts: string[] = [];
					const formFields: IDataObject = {};
					const body: IDataObject = {};
					let wholeBodyArray: unknown[] | undefined;
					let wholeBodyScalar: string | number | boolean | undefined;
					let path = '/user/login';
					qs['username'] = this.getNodeParameter('username', i);
					qs['password'] = this.getNodeParameter('password', i);
					if (cookieParts.length > 0) {
						headers['Cookie'] = cookieParts.join('; ');
					}
					responseData = (await apiRequest.call(this, 'GET', path, wholeBodyArray ?? wholeBodyScalar ?? body, qs, headers, formFields, '')) as IDataObject;
				}
				if (operation === 'logoutUser') {
					matched = true;
					const qs: IDataObject = {};
					const headers: IDataObject = {};
					const cookieParts: string[] = [];
					const formFields: IDataObject = {};
					const body: IDataObject = {};
					let wholeBodyArray: unknown[] | undefined;
					let wholeBodyScalar: string | number | boolean | undefined;
					let path = '/user/logout';
					if (cookieParts.length > 0) {
						headers['Cookie'] = cookieParts.join('; ');
					}
					responseData = (await apiRequest.call(this, 'GET', path, wholeBodyArray ?? wholeBodyScalar ?? body, qs, headers, formFields, 'n8nNodesOpenapiPetstoreApiKeyCredentials')) as IDataObject;
				}
				if (operation === 'updateUser') {
					matched = true;
					const qs: IDataObject = {};
					const headers: IDataObject = {};
					const cookieParts: string[] = [];
					const formFields: IDataObject = {};
					const body: IDataObject = {};
					let wholeBodyArray: unknown[] | undefined;
					let wholeBodyScalar: string | number | boolean | undefined;
					let path = '/user/{username}';
					path = path.replace('{' + 'username' + '}', encodeURIComponent(this.getNodeParameter('username', i) as string));
					body['id'] = this.getNodeParameter('id', i);
					body['username'] = this.getNodeParameter('username_f02988d6', i);
					body['firstName'] = this.getNodeParameter('firstName', i);
					body['lastName'] = this.getNodeParameter('lastName', i);
					body['email'] = this.getNodeParameter('email', i);
					body['password'] = this.getNodeParameter('password', i);
					body['phone'] = this.getNodeParameter('phone', i);
					body['userStatus'] = this.getNodeParameter('userStatus', i);
					if (cookieParts.length > 0) {
						headers['Cookie'] = cookieParts.join('; ');
					}
					responseData = (await apiRequest.call(this, 'PUT', path, wholeBodyArray ?? wholeBodyScalar ?? body, qs, headers, formFields, 'n8nNodesOpenapiPetstoreApiKeyCredentials')) as IDataObject;
				}
			}

			if (!matched) {
				throw new NodeOperationError(this.getNode(), `Unsupported resource/operation combination: resource=${resource}, operation=${operation}`, { itemIndex: i });
			}

			returnData.push(responseData);
		}

		return [this.helpers.returnJsonArray(returnData)];
	}
}
