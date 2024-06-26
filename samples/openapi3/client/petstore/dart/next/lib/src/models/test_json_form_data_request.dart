// Model def

import 'package:openapi/_internal.dart';


part 'test_json_form_data_request.reflection.dart';
part 'test_json_form_data_request.serialization.dart';


/// TestJsonFormDataRequestMixin
///
/// Properties:
/// * [param] - field1
/// * [param2] - field2
mixin TestJsonFormDataRequestMixin on
  $OpenApiObjectMixin {
  
            String
 get param;

            String
 get param2;
  
}

/// TestJsonFormDataRequest
///
/// Properties:
/// * [param] - field1
/// * [param2] - field2
class TestJsonFormDataRequest with
$OpenApiObjectMixin,


TestJsonFormDataRequestMixin {
  @override
  
            String
 param;
  @override
  
            String
 param2;

  AdditionalProperties<Object
?> additionalProperties;

  

  TestJsonFormDataRequest.$all({
        required this.param,
    required this.param2,
    required this.additionalProperties,
    
  });

  TestJsonFormDataRequest({
    required  this.param     ,
required  this.param2     ,
    this.additionalProperties = const AdditionalProperties(),
    
  });

  static const $reflection = TestJsonFormDataRequestReflection.instance;

  @override
  bool validate() {
    return super.validate();
  }


  Map<String, dynamic> toMap() {
    return _$TestJsonFormDataRequestToMap(this);
  }
  factory TestJsonFormDataRequest.fromMap(Map<String, dynamic> src) {
    return _$TestJsonFormDataRequestFromMap(src);
  }
  static TestJsonFormDataRequest? fromMapOrNull(Map<String, dynamic>? src) {
    if (src == null) {
      return null;
    }
    return TestJsonFormDataRequest.fromMap(src);
  }
  static bool canFromMap(Map<String, dynamic>? src) {
    if (src  == null) {
      return false;
    }
    return _$TestJsonFormDataRequestCanFromMap(src);
  }


  /// Deserializes a primitive Object (num, String, List, Map).
  factory TestJsonFormDataRequest.deserialize(Object? src) {
    return _$TestJsonFormDataRequestDeserialize(src);
  }
  static TestJsonFormDataRequest? deserializeOrNull(Object? src) {
    if (src == null) {
      return null;
    }
    return TestJsonFormDataRequest.deserialize(src);
  }
  /// Checks if a primitive Object (num, String, List, Map) can be deserialized.
  static bool canDeserialize(Object? src) {
    return _$TestJsonFormDataRequestCanDeserialize(src);
  }
  /// Serializes to a primitive Object (num, String, List, Map).
  Object? serialize() {
    return _$TestJsonFormDataRequestSerialize(this);
  }
}




