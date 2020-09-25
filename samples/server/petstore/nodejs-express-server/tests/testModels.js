const {
  describe, it,
} = require('mocha');
const chai = require('chai');
const chaiAsPromised = require('chai-as-promised');
const Pet = require('../models/Pet');
const logger = require('./logger');

chai.use(chaiAsPromised);
chai.should();

describe('Model tests, checking that they are created correctly, and throw the expected message when not', () => {
  it('Should create a model for Pet, according to openapi.yaml definition, and throw errors if fails',
    async () => {
      const photoUrls = ['petPhoto1.jpg', 'petPhoto2.jpg', 'petPhoto3.jpg'];
      const name = 'petName';
      const id = 0;
      const category = { id: 1, name: 'categoryName' };
      const tags = [{ id: 2, name: 'tagName1' }, { id: 3, name: 'tagName2' }];
      const status = 'available';
      const pet = new Pet(photoUrls, name, id, tags, status, category);
      pet.id.should.equal(id);
      pet.name.should.equal(name);
      pet.category.id.should.equal(category.id);
      pet.category.name.should.equal(category.name);
      pet.tags.length.should.equal(tags.length);
      pet.status.should.equal(status);
    });
});
