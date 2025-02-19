const ono = require('ono');
const Model = require('./Model');
const Tag = require('./Tag');
const Category = require('./Category');

class Pet {
  constructor(photoUrls, name, id, tags, status, category) {
    const validationErrors = Model.validateModel(Pet, {
      photoUrls, name, id, tags, status, category,
    });
    if (validationErrors.length === 0) {
      this.photoUrls = photoUrls;
      this.name = name;
      this.id = id;
      this.tags = tags.map(t => new Tag(t.id, t.name));
      this.status = status;
      this.category = new Category(category.id, category.name);
    } else {
      throw ono('Tried to create an invalid Pet instance', { errors: validationErrors });
    }
  }
}

Pet.types = {
  photoUrls: 'array',
  name: 'string',
  id: 'integer',
  tags: 'array',
  status: 'string',
  category: 'object',
};

Pet.required = ['name', 'photoUrls'];

module.exports = Pet;
