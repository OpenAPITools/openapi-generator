part of openapi.api;

class OneOfTest {
  dynamic instance;

  // default constructor
  OneOfTest({
  });

  @override
  String toString() {
    // TODO
  }

  OneOfTest.fromJson(Map<String, dynamic> json) {
    // TODO
    // loop through models/primitive types defined in  Pet  User 
    // and make sure the payload `json` deserializes to one and only one schema defined in oneOf
  }

  Map<String, dynamic> toJson() {
    // TOOD there should be a class member/property called "instance"
    // which is dynamic type and it stores the actual instance of the schema defined in oneOf
    // if oneOf is [Dog, Cat], then the instance can store either an instance of Dog or Cat
  }
}

