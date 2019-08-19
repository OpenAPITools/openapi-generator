const ono = require('ono');
const Model = require('./Model');

class UpdatePetWithFormModel {
  constructor(name, status) {
    const validationErrors = Model.validateModel(UpdatePetWithFormModel, {
      name, status,
    });
    if (validationErrors.length === 0) {
      this.name = name;
      this.status = status;
    } else {
      throw ono('Tried to create an invalid UpdatePetWithFormModel instance', { errors: validationErrors });
    }
  }
}

UpdatePetWithFormModel.types = {
  name: 'string',
  status: 'string',
};

UpdatePetWithFormModel.required = [];

module.exports = UpdatePetWithFormModel;
