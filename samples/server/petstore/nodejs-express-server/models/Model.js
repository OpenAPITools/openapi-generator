class Model {
  static validateModel(modelClass, variables) {
    const invalidArray = [];
    Object.entries(variables).forEach(([key, value]) => {
      const typeToCheck = modelClass.types[key];
      switch (typeToCheck) {
        case 'string':
          if (!(typeof value === 'string' || value instanceof String)) {
            invalidArray.push({ key, expectedType: typeToCheck, value });
          }
          break;
        case 'number':
        case 'integer':
          if (!(typeof value === 'number' && !Number.isNaN(value))) {
            invalidArray.push({ key, expectedType: typeToCheck, value });
          }
          break;
        case 'array':
          if (!(value && typeof value === 'object' && value.constructor === Array)) {
            invalidArray.push({ key, expectedType: typeToCheck, value });
          }
          break;
        case 'object':
          if (!(value && typeof value === 'object' && value.constructor === Object)) {
            invalidArray.push({ key, expectedType: typeToCheck, value });
          }
          break;
        case 'boolean':
          if (!(typeof value === 'boolean')) {
            invalidArray.push({ key, expectedType: typeToCheck, value });
          }
          break;
        default:
          break;
      }
    });
    modelClass.required.forEach((requiredFieldName) => {
      if (variables[requiredFieldName] === undefined || variables[requiredFieldName] === '') {
        invalidArray.push(
          { field: requiredFieldName, required: true, value: variables[requiredFieldName] },
        );
      }
    });
    return invalidArray;
  }
}
module.exports = Model;
