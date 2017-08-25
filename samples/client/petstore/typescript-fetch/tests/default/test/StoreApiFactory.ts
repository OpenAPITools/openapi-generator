import { expect } from 'chai';
import { StoreApiFactory } from '@swagger/typescript-fetch-petstore';
import { Configuration } from '@swagger/typescript-fetch-petstore';

let config: Configuration;

before(function () {
    config = new Configuration();
    config.accessToken = "foobar";
    config.apiKey = (securityName: string) => {
        // for multiple apiKey security
        if (securityName === "api_key") {
            return "foobar";
        }
        return;
    };
    config.username = "foo";
    config.password = "bar";
});

describe('StoreApiFactory', function () {
    function runSuite(description: string, requestOptions?: any): void {

        describe(description, () => {

            it('should get inventory', function () {
                return StoreApiFactory(config).getInventory(requestOptions).then((result: { [key: string]: number }) => {
                    expect(Object.keys(result)).to.not.be.empty;
                });
            });

        });
    }

    runSuite('without custom request options');

    runSuite('with custom request options', {
        credentials: 'include',
        mode: 'cors'
    });

});
