import { enableProdMode, provideZoneChangeDetection } from '@angular/core'
import { bootstrapApplication } from '@angular/platform-browser'

import { AppComponent } from './app/app.component'
import { environment } from './environments/environment'
import { ConfigurationParameters, provideApi } from '@swagger/typescript-angular-petstore'
import { provideHttpClient, withInterceptors } from '@angular/common/http'
import { fakePetstoreBackendInterceptorFn } from './test/fakeBackend'

if (environment.production) {
  enableProdMode()
}

export const apiConfigurationParams: ConfigurationParameters = {
  credentials: { api_key: 'foobar' }
}

bootstrapApplication(AppComponent, {
  providers: [
    provideZoneChangeDetection(),
    provideApi(apiConfigurationParams),
  ]
}).catch(err => { console.log(err) })
