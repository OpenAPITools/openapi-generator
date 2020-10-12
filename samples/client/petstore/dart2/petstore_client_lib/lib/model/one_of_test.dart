part of openapi.api;

class OneOfTest {
  /// Can be [Pet], [User], 
  dynamic _instance;

  // default constructor
  OneOfTest();

  set instance(dynamic instance) {
    if (!(instance == null || instance is Pet || instance is User))
      throw ArgumentError("${instance.runtimeType} is not a valid type for OneOfTest");
    _instance = instance;
  }

  dynamic get instance => _instance;

  @override
  String toString() {
    return _instance.toString();
  }

  OneOfTest.fromJson(Map<String, dynamic> json) {
    if (json == null) return;
    // TODO primitives, lists, maps
    try {
      _instance = Pet.fromJson(json);
    } on ArgumentError {
    }
    try {
      _instance = User.fromJson(json);
    } on ArgumentError {
    }
  }

  Map<String, dynamic> toJson() {
    // TODO primitives, lists, maps
    return _instance.toJson();
  }
}

