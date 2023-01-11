## ts-petstore-client@1.0.0

This generator creates TypeScript/JavaScript client that utilizes fetch-api.

### Building

To build and compile the typescript sources to javascript use:

```shell
npm install
npm run build
```

### Publishing

First build the package then run ```npm publish```

### Consuming

navigate to the folder of your consuming project and run one of the following commands.

_published:_

```shell
npm install ts-petstore-client@1.0.0 --save
```

_unPublished (not recommended):_

```shell
npm install PATH_TO_GENERATED_PACKAGE --save
```