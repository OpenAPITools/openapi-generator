import { expect } from 'chai';
import { StoreApi } from '@swagger/typescript-fetch-petstore';
import { config } from '../configuration';

describe('StoreApi', function () {

    function runSuite(description: string): void {

        describe(description, () => {
            let api: StoreApi;

            beforeEach(function () {
                api = new StoreApi(config);
            });

            it('should get inventory', function () {
                return api.getInventory().then((result: { [key: string]: number }) => {
                    expect(Object.keys(result)).to.not.be.empty;
                });
            });

        });
    }

    runSuite('without custom request options');

});
