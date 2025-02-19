const ono = require('ono');
const Model = require('./Model');

class UploadFileModel {
  constructor(additionalMetadata, file) {
    const validationErrors = Model.validateModel(UploadFileModel, {
      additionalMetadata, file,
    });
    if (validationErrors.length === 0) {
      this.additionalMetadata = additionalMetadata;
      this.file = file;
    } else {
      throw ono('Tried to create an invalid UploadFileModel instance', { errors: validationErrors });
    }
  }
}

UploadFileModel.types = {
  additionalMetadata: 'string',
  file: 'string',
};

UploadFileModel.required = [];

module.exports = UploadFileModel;
