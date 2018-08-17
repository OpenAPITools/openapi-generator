import { expect } from 'chai';
import { PetApiFactory, Pet, Category } from '@swagger/typescript-fetch-petstore';
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

describe('PetApiFactory', () => {


    function runSuite(description: string, requestOptions?: any): void {

        describe(description, () => {

            const fixture: Pet = createTestFixture();

            it('should add and delete Pet', () => {
                return PetApiFactory(config).addPet(fixture, requestOptions).then(() => {
                });
            });

            it('should get Pet by ID', () => {
                return PetApiFactory(config).getPetById(fixture.id, requestOptions).then((result: Pet) => {
                    return expect(result).to.deep.equal(fixture);
                });
            });

            it('should update Pet by ID', () => {
                return PetApiFactory(config).getPetById(fixture.id, requestOptions).then((result: Pet) => {
                    result.name = 'newname';
                    return PetApiFactory(config).updatePet(result, requestOptions).then(() => {
                        return PetApiFactory(config).getPetById(fixture.id, requestOptions).then((result: Pet) => {
                            return expect(result.name).to.deep.equal('newname');
                        });
                    });
                });
            });

            it('should delete Pet', () => {
                return PetApiFactory(config).deletePet(fixture.id, requestOptions);
            });

            it('should not contain deleted Pet', () => {
                return PetApiFactory(config).getPetById(fixture.id, requestOptions).then((result: Pet) => {
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
