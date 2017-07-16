import {expect} from 'chai';
import {StoreApi} from 'typescript-fetch-api';

describe('StoreApi', function() {

  function runSuite(description: string, requestOptions?: any): void {

    describe(description, () => {
      let api: StoreApi;

      beforeEach(function() {
        api = new StoreApi();
      });

      it('should get inventory', function() {
        return api.getInventory(requestOptions).then((result: { [key: string]: number }) => {
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
