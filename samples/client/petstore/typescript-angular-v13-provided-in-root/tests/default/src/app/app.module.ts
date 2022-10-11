import { BrowserModule } from '@angular/platform-browser';
import { NgModule } from '@angular/core';
import { HttpClientModule } from '@angular/common/http';
import {
  ApiModule,
  Configuration,
  ConfigurationParameters
} from '@swagger/typescript-angular-petstore'

import { AppComponent } from './app.component';

export const apiConfigurationParams: ConfigurationParameters = {
  apiKeys: { api_key: "foobar" }
};

export const apiConfig = new Configuration(apiConfigurationParams);

export function getApiConfig() {
  return apiConfig;
}

@NgModule({
  declarations: [
    AppComponent,
  ],
  imports: [
    BrowserModule,
    HttpClientModule,
    ApiModule.forRoot(getApiConfig),
  ],
  providers: [
  ],
  bootstrap: [AppComponent]
})
export class AppModule { }

