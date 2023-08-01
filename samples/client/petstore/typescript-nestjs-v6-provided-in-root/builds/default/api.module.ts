import { DynamicModule, HttpService, HttpModule, Module, Global } from '@nestjs/common';
import { AsyncConfiguration, Configuration, ConfigurationFactory } from './configuration';

import { PetService } from './api/pet.service';
import { StoreService } from './api/store.service';
import { UserService } from './api/user.service';

@Global()
@Module({
  imports:      [ HttpModule ],
  exports:      [
    PetService,
    StoreService,
    UserService
  ],
  providers: [
    PetService,
    StoreService,
    UserService
  ]
})
export class ApiModule {
    public static forRoot(configurationFactory: () => Configuration): DynamicModule {
        return {
            module: ApiModule,
            providers: [ { provide: Configuration, useFactory: configurationFactory } ]
        };
    }

    /**
     * Register the module asynchronously.
     */
    static forRootAsync(options: AsyncConfiguration): DynamicModule {
        const providers = [...this.createAsyncProviders(options)];
        return {
            module: ApiModule,
            imports: options.imports || [],
            providers,
            exports: providers,
        };
    }

    private static createAsyncProviders(options: AsyncConfiguration): Provider[] {
        if (options.useExisting || options.useFactory) {
            return [this.createAsyncConfigurationProvider(options)];
        }
        return [
            this.createAsyncConfigurationProvider(options),
            {
                provide: options.useClass,
                useClass: options.useClass,
            },
        ];
    }

    private static createAsyncConfigurationProvider(
        options: AsyncConfiguration,
    ): Provider {
        if (options.useFactory) {
            return {
                provide: Configuration,
                useFactory: options.useFactory,
                inject: options.inject || [],
            };
        }
        return {
            provide: Configuration,
            useFactory: async (optionsFactory: ConfigurationFactory) =>
                await optionsFactory.createConfiguration(),
            inject: [options.useExisting || options.useClass],
        };
    }

    constructor( httpService: HttpService) { }
}
