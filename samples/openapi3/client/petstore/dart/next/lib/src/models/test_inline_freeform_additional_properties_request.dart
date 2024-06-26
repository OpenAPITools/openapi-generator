// Model def

import 'package:openapi/_internal.dart';


part 'test_inline_freeform_additional_properties_request.reflection.dart';
part 'test_inline_freeform_additional_properties_request.serialization.dart';


/// TestInlineFreeformAdditionalPropertiesRequestMixin
///
/// Properties:
/// * [someProperty] 
mixin TestInlineFreeformAdditionalPropertiesRequestMixin on
  $OpenApiObjectMixin {
  UndefinedWrapper<
            String
> get someProperty;
  
}

/// TestInlineFreeformAdditionalPropertiesRequest
///
/// Properties:
/// * [someProperty] 
class TestInlineFreeformAdditionalPropertiesRequest with
$OpenApiObjectMixin,


TestInlineFreeformAdditionalPropertiesRequestMixin {
  @override
  UndefinedWrapper<
            String
> someProperty;

  AdditionalProperties<Object
?> additionalProperties;

  

  TestInlineFreeformAdditionalPropertiesRequest.$all({
        required this.someProperty,
    required this.additionalProperties,
    
  });

  TestInlineFreeformAdditionalPropertiesRequest({
      this.someProperty = const UndefinedWrapper
        .undefined()
,
    this.additionalProperties = const AdditionalProperties(),
    
  });

  static const $reflection = TestInlineFreeformAdditionalPropertiesRequestReflection.instance;

  @override
  bool validate() {
    return super.validate();
  }


  Map<String, dynamic> toMap() {
    return _$TestInlineFreeformAdditionalPropertiesRequestToMap(this);
  }
  factory TestInlineFreeformAdditionalPropertiesRequest.fromMap(Map<String, dynamic> src) {
    return _$TestInlineFreeformAdditionalPropertiesRequestFromMap(src);
  }
  static TestInlineFreeformAdditionalPropertiesRequest? fromMapOrNull(Map<String, dynamic>? src) {
    if (src == null) {
      return null;
    }
    return TestInlineFreeformAdditionalPropertiesRequest.fromMap(src);
  }
  static bool canFromMap(Map<String, dynamic>? src) {
    if (src  == null) {
      return false;
    }
    return _$TestInlineFreeformAdditionalPropertiesRequestCanFromMap(src);
  }


  /// Deserializes a primitive Object (num, String, List, Map).
  factory TestInlineFreeformAdditionalPropertiesRequest.deserialize(Object? src) {
    return _$TestInlineFreeformAdditionalPropertiesRequestDeserialize(src);
  }
  static TestInlineFreeformAdditionalPropertiesRequest? deserializeOrNull(Object? src) {
    if (src == null) {
      return null;
    }
    return TestInlineFreeformAdditionalPropertiesRequest.deserialize(src);
  }
  /// Checks if a primitive Object (num, String, List, Map) can be deserialized.
  static bool canDeserialize(Object? src) {
    return _$TestInlineFreeformAdditionalPropertiesRequestCanDeserialize(src);
  }
  /// Serializes to a primitive Object (num, String, List, Map).
  Object? serialize() {
    return _$TestInlineFreeformAdditionalPropertiesRequestSerialize(this);
  }
}




