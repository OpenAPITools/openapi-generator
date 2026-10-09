# @

Operations covering each shape of the resource methods generated with withHttpResource.

The version of the OpenAPI document: 1.0.0

## Building

To install the required dependencies and to build the typescript sources run:

```console
npm install
npm run build
```

## Publishing

First build the package then run `npm publish dist` (don't forget to specify the `dist` folder!)

## Consuming

Navigate to the folder of your consuming project and run one of next commands.

_published:_

```console
npm install @ --save
```

_without publishing (not recommended):_

```console
npm install PATH_TO_GENERATED_PACKAGE/dist.tgz --save
```

_It's important to take the tgz file, otherwise you'll get trouble with links on windows_

_using `npm link`:_

In PATH_TO_GENERATED_PACKAGE/dist:

```console
npm link
```

In your project:

```console
npm link 
```

__Note for Windows users:__ The Angular CLI has troubles to use linked npm packages.
Please refer to this issue <https://github.com/angular/angular-cli/issues/8284> for a solution / workaround.
Published packages are not effected by this issue.

### General usage

In your Angular project:

```typescript

import { ApplicationConfig } from '@angular/core';
import { provideHttpClient } from '@angular/common/http';
import { provideApi } from '';

export const appConfig: ApplicationConfig = {
    providers: [
        // ...
        provideHttpClient(),
        provideApi()
    ],
};
```

**NOTE**
If you're still using `AppModule` and haven't [migrated](https://angular.dev/reference/migrations/standalone) yet, you can still import an Angular module:
```typescript
import { ApiModule } from '';
```

If different from the generated base path, during app bootstrap, you can provide the base path to your service.

```typescript
import { ApplicationConfig } from '@angular/core';
import { provideHttpClient } from '@angular/common/http';
import { provideApi } from '';

export const appConfig: ApplicationConfig = {
    providers: [
        // ...
        provideHttpClient(),
        provideApi('http://localhost:9999')
    ],
};
```

```typescript
// with a custom configuration
import { ApplicationConfig } from '@angular/core';
import { provideHttpClient } from '@angular/common/http';
import { provideApi } from '';

export const appConfig: ApplicationConfig = {
    providers: [
        // ...
        provideHttpClient(),
        provideApi({
            withCredentials: true,
            username: 'user',
            password: 'password'
        })
    ],
};
```

```typescript
// with factory building a custom configuration
import { ApplicationConfig } from '@angular/core';
import { provideHttpClient } from '@angular/common/http';
import { provideApi, Configuration } from '';

export const appConfig: ApplicationConfig = {
    providers: [
        // ...
        provideHttpClient(),
        {
            provide: Configuration,
            useFactory: (authService: AuthService) => new Configuration({
                    basePath: 'http://localhost:9999',
                    withCredentials: true,
                    username: authService.getUsername(),
                    password: authService.getPassword(),
            }),
            deps: [AuthService],
            multi: false
        }
    ],
};
```

### Using multiple OpenAPI files / APIs

In order to use multiple APIs generated from different OpenAPI files,
you can create an alias name when importing the modules
in order to avoid naming conflicts:

```typescript
import { provideApi as provideUserApi } from 'my-user-api-path';
import { provideApi as provideAdminApi } from 'my-admin-api-path';
import { HttpClientModule } from '@angular/common/http';
import { environment } from '../environments/environment';

export const appConfig: ApplicationConfig = {
    providers: [
        // ...
        provideHttpClient(),
        provideUserApi(environment.basePath),
        provideAdminApi(environment.basePath),
    ],
};
```

### Signal-based resources

Every `GET` operation without a request body also has a `...Resource` method, next to its Observable method,
which returns an Angular [`HttpResourceRef`](https://angular.dev/api/common/http/HttpResourceRef)
(see [`httpResource`](https://angular.dev/guide/http/http-resource)). The method is skipped, with a warning at
generation time, when its name would collide with another operation:

```typescript
import { Component, computed, inject, input } from '@angular/core';
import { PetService } from '';

@Component({
    selector: 'app-pet',
    templateUrl: './pet.component.html',
})
export class PetComponent {
    readonly petId = input.required<number>();

    // requested again whenever petId changes
    readonly pet = inject(PetService).getPetByIdResource(() => ({ petId: this.petId() }));

    readonly name = computed(() => this.pet.hasValue() ? this.pet.value().name : '');
}
```

- The first argument returns the request parameters and is called in a reactive context: every signal it reads
  sends a new request when it changes. While it returns `undefined`, or a required parameter is `null` or `undefined`,
  the resource stays idle. Operations without parameters take no such argument, and it can be left out when all
  parameters are optional.
- Call the method in an injection context (a field initializer or a constructor), or pass an `injector` in the options.
  The resource belongs to that injector: it is destroyed with it, and it sends its request through the `HttpClient`
  of that injector, so the interceptors of the caller apply.
- The credentials of the `Configuration` are read each time the request is built, so a credential
  function that reads a signal sends a new request when the signal changes. Other credential changes need `reload()`.
- `transferCache` defaults to `true`, as for the Observable methods. Angular's transfer cache skips requests with an
  `Authorization` header unless `includeRequestsWithAuthHeaders` is set in `withHttpTransferCacheOptions`.
- Resources are generated for reads only. A resource cancels its pending request when its parameters change, which
  is not safe for a write, so operations that change data keep only their Observable method.

The `withHttpResource` option requires Angular 20 or later. `httpResource` is stable since Angular 22 and
experimental in Angular 20 and 21.

### Customizing path parameter encoding

Without further customization, only [path-parameters][parameter-locations-url] of [style][style-values-url] 'simple'
and Dates for format 'date-time' are encoded correctly.

Other styles (e.g. "matrix") are not that easy to encode
and thus are best delegated to other libraries (e.g.: [@honoluluhenk/http-param-expander]).

To implement your own parameter encoding (or call another library),
pass an arrow-function or method-reference to the `encodeParam` property of the Configuration-object
(see [General Usage](#general-usage) above).

Example value for use in your Configuration-Provider:

```typescript
new Configuration({
    encodeParam: (param: Param) => myFancyParamEncoder(param),
})
```

[parameter-locations-url]: https://github.com/OAI/OpenAPI-Specification/blob/main/versions/3.1.0.md#parameter-locations
[style-values-url]: https://github.com/OAI/OpenAPI-Specification/blob/main/versions/3.1.0.md#style-values
[@honoluluhenk/http-param-expander]: https://www.npmjs.com/package/@honoluluhenk/http-param-expander
