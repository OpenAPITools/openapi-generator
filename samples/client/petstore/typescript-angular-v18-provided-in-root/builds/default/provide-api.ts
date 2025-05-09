import { makeEnvironmentProviders } from "@angular/core";
import { Configuration, ConfigurationParameters } from './configuration';
import { BASE_PATH } from './variables';

export function provideApi(configOrBasePath: string | ConfigurationParameters) {
    return makeEnvironmentProviders([
        typeof configOrBasePath === "string"
            ? { provide: BASE_PATH, useValue: configOrBasePath }
            : {
                provide: Configuration,
                useValue: new Configuration({ ...configOrBasePath }),
            },
    ]);
}