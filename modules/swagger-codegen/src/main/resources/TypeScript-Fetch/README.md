# TypeScript-Fetch

This generator creates TypeScript/JavaScript client that utilizes [Fetch API](https://fetch.spec.whatwg.org/). The codegen Node module can be used in the following environments: 

* Node.JS
* Webpack
* Browserify

It is usable in both TypeScript and JavaScript. In TypeScript, the definition should be automatically resolved via `typings` in `package.json`. ([Reference](http://www.typescriptlang.org/docs/handbook/typings-for-npm-packages.html))

### Installation ###

`swagger-codegen` does not generate JavaScript directly. The codegen Node module comes with `package.json` that bundles `typescript` and `typings` so it can self-compile. The self-compile is normally run automatically via the `npm` `postinstall` script of `npm install`. 

CAVEAT: Due to [privilege implications](https://docs.npmjs.com/misc/scripts#user), `npm` may skip `postinstall` script if the user is `root`. You would need to manually invoke `npm install` or `npm run postinstall` for the codegen module if that's the case. 

#### NPM repository ###
If you remove `"private": true` from `package.json`, you may publish the module to NPM. In which case, you would be able to install the module as any other NPM module. 

It maybe useful to use [scoped packages](https://docs.npmjs.com/misc/scope).

#### NPM install local file ###
You should be able to directly install the module using `npm install file:///codegen_path`. 

NOTES: If you do `npm install file:///codegen_path --save` NPM might convert your path to relative path, maybe have adverse affect. `npm install` and `npm shrinkwrap` may misbehave if the installation path is not absolute.

#### direct copy/symlink ###
You may also simply copy or symlink the codegen into a directly under your project. The syntax of the usage would differ if you take this route. (See below)

### Usage ###
With ES6 module syntax, the following syntaxes are supported: 
```
import * as localName from 'npmName';
import {operationId} from 'npmName';

import * as localName from './symlinkDir';
import {operationId} from './symlinkDir';
```
With CommonJS, the following syntaxes are supported: 
```
import localName = require('npmName');

import localName = require('./symlinkDir')';
```