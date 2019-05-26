class Service {
  constructor() {
    this.a = 22;
  }

  static rejectResponse(error, code = 500) {
    return { error, code };
  }

  static successResponse(payload, code = 200) {
    return { payload, code };
  }
}

module.exports = Service;
