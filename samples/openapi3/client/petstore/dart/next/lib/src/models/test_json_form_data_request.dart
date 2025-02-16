// Model def

import 'package:petstore_api/_internal.dart';


part 'test_json_form_data_request.reflection.dart';


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
    AdditionalProperties<Object
?>? additionalProperties,
    
  }) : additionalProperties = additionalProperties ?? {};

  static const $reflection = TestJsonFormDataRequestReflection.instance;
  TestJsonFormDataRequestReflection get $classReflection => $reflection;

  @override
  bool validate() {
    return super.validate();
  }

  factory TestJsonFormDataRequest.deserialize(Object? src, [SerializationContext context = const SerializationContext.json(),]) {
    return $reflection.deserialize(src, context);
  }

  static bool canDeserialize(Object? src, [SerializationContext context = const SerializationContext.json(),]) {
    return $reflection.canDeserialize(src, context);
  }

  Object? serialize([SerializationContext context = const SerializationContext.json(),]) {
    return $reflection.serialize(this, context);
  }

  TestJsonFormDataRequest clone() {
    return $reflection.clone(this);
  }
}








