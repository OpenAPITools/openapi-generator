## @openapitools/typescript-redux-query-petstore@1.0.0

This generator creates TypeScript/JavaScript client that utilizes [redux-query](https://amplitude.github.io/redux-query/).
The generated Node module does not depend on ReactJS specifically.

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
npm install @openapitools/typescript-redux-query-petstore@1.0.0 --save
```

_unPublished (not recommended):_

```
npm install PATH_TO_GENERATED_PACKAGE --save
```
