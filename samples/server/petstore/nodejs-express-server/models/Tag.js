const ono = require('ono');
const Model = require('./Model');

class Tag {
  constructor(id, name) {
    const validationErrors = Model.validateModel(Tag,
      { name, id });
    if (validationErrors.length === 0) {
      this.name = name;
      this.id = id;
    } else {
      throw ono('Tried to create an invalid Tag instance', { errors: validationErrors });
    }
  }
}

Tag.types = {
  name: 'string',
  id: 'integer',
};

Tag.required = [];

module.exports = Tag;
