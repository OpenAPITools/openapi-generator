const ono = require('ono');
const Model = require('./Model');

class Tag {
  constructor(name, id) {
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

module.exports = Tag;
