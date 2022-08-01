## @openapitools/typescript-nestjs-petstore@1.0.0

### Building

To install the required dependencies and to build the typescript sources run:
```
npm install
npm run build
```

#### General usage

In your Nestjs project:


```
// without configuring providers
import { ApiModule } from '@openapitools/typescript-nestjs-petstore';
import { HttpModule } from '@nestjs/common';

@Module({
    imports: [
        ApiModule,
        HttpModule
    ],
    providers: []
})
export class AppModule {}
```

```
// configuring providers
import { ApiModule, Configuration, ConfigurationParameters } from '@openapitools/typescript-nestjs-petstore';

export function apiConfigFactory (): Configuration => {
  const params: ConfigurationParameters = {
    // set configuration parameters here.
  }
  return new Configuration(params);
}

@Module({
    imports: [ ApiModule.forRoot(apiConfigFactory) ],
    declarations: [ AppComponent ],
    providers: [],
    bootstrap: [ AppComponent ]
})
export class AppModule {}
```

```
import { DefaultApi } from '@openapitools/typescript-nestjs-petstore';

export class AppComponent {
    constructor(private apiGateway: DefaultApi) { }
}
```

Note: The ApiModule a dynamic module and instantiated once app wide.
This is to ensure that all services are treated as singletons.

#### Using multiple swagger files / APIs / ApiModules
In order to use multiple `ApiModules` generated from different swagger files,
you can create an alias name when importing the modules
in order to avoid naming conflicts:
```
import { ApiModule } from 'my-api-path';
import { ApiModule as OtherApiModule } from 'my-other-api-path';
import { HttpModule } from '@nestjs/common';

@Module({
  imports: [
    ApiModule,
    OtherApiModule,
    HttpModule
  ]
})
export class AppModule {

}
```


### Set service base path
If different than the generated base path, during app bootstrap, you can provide the base path to your service.

```
import { BASE_PATH } from '@openapitools/typescript-nestjs-petstore';

bootstrap(AppComponent, [
    { provide: BASE_PATH, useValue: 'https://your-web-service.com' },
]);
```
or

```
import { BASE_PATH } from '@openapitools/typescript-nestjs-petstore';

@Module({
    imports: [],
    declarations: [ AppComponent ],
    providers: [ provide: BASE_PATH, useValue: 'https://your-web-service.com' ],
    bootstrap: [ AppComponent ]
})
export class AppModule {}
```


#### Using @nestjs/cli
First extend your `src/environments/*.ts` files by adding the corresponding base path:

```
export const environment = {
  production: false,
  API_BASE_PATH: 'http://127.0.0.1:8080'
};
```

In the src/app/app.module.ts:
```
import { BASE_PATH } from '@openapitools/typescript-nestjs-petstore';
import { environment } from '../environments/environment';

@Module({
  declarations: [
    AppComponent
  ],
  imports: [ ],
  providers: [
    {
      provide: 'BASE_PATH',
      useValue: environment.API_BASE_PATH
    }
  ]
})
export class AppModule { }
```
