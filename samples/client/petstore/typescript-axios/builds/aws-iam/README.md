## @openapitools/typescript-axios-with-aws-iam@1.0.0

This generator creates TypeScript/JavaScript client that utilizes [axios](https://github.com/axios/axios). The generated Node module can be used in the following environments:

Environment
* Node.js
* Webpack
* Browserify

Language level
* ES5 - you must have a Promises/A+ library installed
* ES6

Module system
* CommonJS
* ES6 module system

It can be used in both TypeScript and JavaScript. In TypeScript, the definition will be automatically resolved via `package.json`. ([Reference](https://www.typescriptlang.org/docs/handbook/declaration-files/consumption.html))

### Building

To build and compile the typescript sources to javascript use:
```
npm install
npm run build
```

### Publishing

First build the package then run `npm publish`

### Consuming

navigate to the folder of your consuming project and run one of the following commands.

_published:_

```
npm install @openapitools/typescript-axios-with-aws-iam@1.0.0 --save
```

_unPublished (not recommended):_

```
npm install PATH_TO_GENERATED_PACKAGE --save
```

### Documentation for API Endpoints

All URIs are relative to *https://abc123.execute-api.us-east-1.amazonaws.com/prod*

Class | Method | HTTP request | Description
------------ | ------------- | ------------- | -------------
*DefaultApi* | [**createPet**](docs/DefaultApi.md#createpet) | **POST** /pet | Create a new pet
*DefaultApi* | [**getInventory**](docs/DefaultApi.md#getinventory) | **GET** /store/inventory | Returns pet inventories by status
*DefaultApi* | [**getPetById**](docs/DefaultApi.md#getpetbyid) | **GET** /pet | Find pet by ID


### Documentation For Models

 - [Pet](docs/Pet.md)


<a id="documentation-for-authorization"></a>
## Documentation For Authorization


Authentication schemes defined for the API:
<a id="aws_iam"></a>
### aws_iam

- **Type**: API key
- **API key parameter name**: Authorization
- **Location**: HTTP header

<a id="sigv4_auth"></a>
### sigv4_auth

- **Type**: API key
- **API key parameter name**: Authorization
- **Location**: HTTP header


<a id="aws-v4-signature"></a>
### AWS V4 Signature

This client supports AWS Signature Version 4 for authenticating requests to AWS services.

#### Configuration

```typescript
import { Configuration } from '@openapitools/typescript-axios-with-aws-iam';

const configuration = new Configuration({
  awsv4: {
    credentials: {
      accessKeyId: 'your-access-key-id',
      secretAccessKey: 'your-secret-access-key',
      sessionToken: 'your-session-token' // Optional, for temporary credentials
    },
    options: {
      region: 'us-east-1', // AWS region
      service: 'execute-api' // AWS service name, typically 'execute-api' for API Gateway
    }
  }
});
```

#### Environment Variables

You can also use environment variables for AWS credentials:

```bash
export AWS_ACCESS_KEY_ID=your-access-key-id
export AWS_SECRET_ACCESS_KEY=your-secret-access-key
export AWS_SESSION_TOKEN=your-session-token  # Optional
```

When environment variables are set, they will be used as fallbacks if not provided in the configuration.

#### Usage Example

```typescript
import { DefaultApi, Configuration } from '@openapitools/typescript-axios-with-aws-iam';

const configuration = new Configuration({
  basePath: 'https://your-api-gateway-id.execute-api.us-east-1.amazonaws.com/stage',
  awsv4: {
    credentials: {
      accessKeyId: process.env.AWS_ACCESS_KEY_ID,
      secretAccessKey: process.env.AWS_SECRET_ACCESS_KEY,
      sessionToken: process.env.AWS_SESSION_TOKEN
    },
    options: {
      region: 'us-east-1',
      service: 'execute-api'
    }
  }
});

const apiInstance = new DefaultApi(configuration);

// All requests will now be signed with AWS V4 signature
apiInstance.someMethod().then((response) => {
  console.log(response.data);
}).catch((error) => {
  console.error(error);
});
```

#### IAM Permissions

Ensure your AWS credentials have the necessary IAM permissions to access the API endpoints. For API Gateway, this typically includes:

```json
{
  "Version": "2012-10-17",
  "Statement": [
    {
      "Effect": "Allow",
      "Action": [
        "execute-api:Invoke"
      ],
      "Resource": "arn:aws:execute-api:region:account-id:api-id/*"
    }
  ]
}
```

