## @swagger/angular2-typescript-petstore@0.0.1

### Building

To build an compile the typescript sources to javascript use:
```
npm install
npm run build
```

### publishing

First build the package than run ```npm publish```

### consuming

navigate to the folder of your consuming project and run one of next commando's.

_published:_

```
npm install @swagger/angular2-typescript-petstore@0.0.1 --save
```

_unPublished (not recommended):_

```
npm install PATH_TO_GENERATED_PACKAGE --save
```

_using `npm link`:_

In PATH_TO_GENERATED_PACKAGE:
```
npm link
```

In your project:
```
npm link @swagger/angular2-typescript-petstore@0.0.1
```

In your Angular project:


```
// without configuring providers
import { ApiModule } from '@swagger/angular2-typescript-petstore';

import { HttpModule } from '@angular/http';

@NgModule({
    imports: [
        ApiModule,
        HttpModule
    ],
    declarations: [ AppComponent ],
    providers: [],
    bootstrap: [ AppComponent ]
})
export class AppModule {}
```

```
// configuring providers
import { ApiModule, Configuration, ConfigurationParameters } from '@swagger/angular2-typescript-petstore';

export function apiConfigFactory (): Configuration => {
  const params: ConfigurationParameters = {
    // set configuration parameters here.
  }
  return new Configuration(params);
}

@NgModule({
    imports: [ ApiModule.forRoot(apiConfigFactory) ],
    declarations: [ AppComponent ],
    providers: [],
    bootstrap: [ AppComponent ]
})
export class AppModule {}
```

```
import { DefaultApi } from '@swagger/angular2-typescript-petstore';

export class AppComponent {
	 constructor(private apiGateway: DefaultApi) { }
}
```

Note: The ApiModule is restricted to being instantiated once app wide.
This is to ensure that all services are treated as singletons.

#### Using multiple swagger files / APIs / ApiModules
In order to use multiple `ApiModules` generated from different swagger files,
you can create an alias name when importing the modules
in order to avoid naming conflicts:
```
import { ApiModule } from 'my-api-path';
import { ApiModule as OtherApiModule } from 'my-other-api-path';

import { HttpModule } from '@angular/http';

@NgModule({
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
import { BASE_PATH } from '@swagger/angular2-typescript-petstore';

bootstrap(AppComponent, [
    { provide: BASE_PATH, useValue: 'https://your-web-service.com' },
]);
```
or

```
import { BASE_PATH } from '@swagger/angular2-typescript-petstore';

@NgModule({
    imports: [],
    declarations: [ AppComponent ],
    providers: [ provide: BASE_PATH, useValue: 'https://your-web-service.com' ],
    bootstrap: [ AppComponent ]
})
export class AppModule {}
```


#### Using @angular/cli
First extend your `src/environments/*.ts` files by adding the corresponding base path:

```
export const environment = {
  production: false,
  API_BASE_PATH: 'http://127.0.0.1:8080'
};
```

In the src/app/app.module.ts:
```
import { BASE_PATH } from '@swagger/angular2-typescript-petstore';
import { environment } from '../environments/environment';

@NgModule({
  declarations: [
    AppComponent
  ],
  imports: [ ],
  providers: [{ provide: BASE_PATH, useValue: environment.API_BASE_PATH }],
  bootstrap: [ AppComponent ]
})
export class AppModule { }
```  