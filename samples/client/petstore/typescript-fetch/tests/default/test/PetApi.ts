import { expect } from 'chai';
import { PetApi, Pet, Category } from '@swagger/typescript-fetch-petstore';

describe('PetApi', () => {

    function runSuite(description: string, requestOptions?: any): void {

        describe(description, () => {

            let api: PetApi;
            const fixture: Pet = createTestFixture();

            beforeEach(() => {
                api = new PetApi();
            });

            it('should add and delete Pet', () => {
                return api.addPet(fixture, requestOptions).then(() => {
                });
            });

            it('should get Pet by ID', () => {
                return api.getPetById(fixture.id, requestOptions).then((result: Pet) => {
                    return expect(result).to.deep.equal(fixture);
                });
            });

            it('should update Pet by ID', () => {
                return api.getPetById(fixture.id, requestOptions).then((result: Pet) => {
                    result.name = 'newname';
                    return api.updatePet(result, requestOptions).then(() => {
                        return api.getPetById(fixture.id, requestOptions).then((result: Pet) => {
                            return expect(result.name).to.deep.equal('newname');
                        });
                    });
                });
            });

            it('should delete Pet', () => {
                return api.deletePet(fixture.id, requestOptions);
            });

            it('should not contain deleted Pet', () => {
                return api.getPetById(fixture.id, requestOptions).then((result: Pet) => {
                    return expect(result).to.not.exist;
                }, (err: any) => {
                    return expect(err).to.exist;
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

function createTestFixture(ts = Date.now()) {
    const category: Category = {
        'id': ts,
        'name': `category${ts}`,
    };

    const pet: Pet = {
        'id': ts,
        'name': `pet${ts}`,
        'category': category,
        'photoUrls': ['http://foo.bar.com/1', 'http://foo.bar.com/2'],
        'status': Pet.StatusEnum.Available,
        'tags': []
    };

    return pet;
};
