part of openapi.api;

class OneOfTest {
  /// Can be [Pet], [User], 
  dynamic _instance;

  // default constructor
  OneOfTest();

  set instance(dynamic instance) {
    if (!(false || instance is Pet || instance is User ))
      throw ArgumentError("${instance.runtimeType} is not a valid type for OneOfTest");
    _instance = instance;
  }

  dynamic get instance => _instance;

  @override
  String toString() {
    return _instance.toString();
  }

  OneOfTest.fromJson(Map<String, dynamic> json) {
    // TODO
    // loop through models/primitive types defined in  Pet  User 
    // and make sure the payload `json` deserializes to one and only one schema defined in oneOf
  }

  Map<String, dynamic> toJson() {
    // TODO primitives, lists, maps
    return _instance.toJson();
  }
}

