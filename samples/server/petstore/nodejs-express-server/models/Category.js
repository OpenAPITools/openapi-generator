const ono = require('ono');
const Model = require('./Model');

class Category {
  constructor(id, name) {
    const validationErrors = (Model.validateModel(Category, { name, id }));
    if (validationErrors.length === 0) {
      this.id = id;
      this.name = name;
    } else {
      throw ono('Tried to create an invalid Category instance', { errors: validationErrors });
    }
  }
}

Category.types = {
  id: 'integer',
  name: 'string',
};

Category.required = [];

module.exports = Category;
