class ApiResponseModel {
  constructor(code, type, message) {
    this.code = code;
    this.type = type;
    this.message = message;
  }
}

module.exports = ApiResponseModel;
