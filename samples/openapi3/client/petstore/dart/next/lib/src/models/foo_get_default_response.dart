// Model def

import 'package:openapi/_internal.dart';


part 'foo_get_default_response.reflection.dart';
part 'foo_get_default_response.serialization.dart';


/// FooGetDefaultResponseMixin
///
/// Properties:
/// * [string] 
mixin FooGetDefaultResponseMixin on
  $OpenApiObjectMixin {
  UndefinedWrapper<
            Foo
> get string;
  
}

/// FooGetDefaultResponse
///
/// Properties:
/// * [string] 
class FooGetDefaultResponse with
$OpenApiObjectMixin,


FooGetDefaultResponseMixin {
  @override
  UndefinedWrapper<
            Foo
> string;

  AdditionalProperties<Object
?> additionalProperties;

  

  FooGetDefaultResponse.$all({
        required this.string,
    required this.additionalProperties,
    
  });

  FooGetDefaultResponse({
      this.string = const UndefinedWrapper
        .undefined()
,
    this.additionalProperties = const AdditionalProperties(),
    
  });

  static const $reflection = FooGetDefaultResponseReflection.instance;

  @override
  bool validate() {
    return super.validate();
  }


  Map<String, dynamic> toMap() {
    return _$FooGetDefaultResponseToMap(this);
  }
  factory FooGetDefaultResponse.fromMap(Map<String, dynamic> src) {
    return _$FooGetDefaultResponseFromMap(src);
  }
  static FooGetDefaultResponse? fromMapOrNull(Map<String, dynamic>? src) {
    if (src == null) {
      return null;
    }
    return FooGetDefaultResponse.fromMap(src);
  }
  static bool canFromMap(Map<String, dynamic>? src) {
    if (src  == null) {
      return false;
    }
    return _$FooGetDefaultResponseCanFromMap(src);
  }


  /// Deserializes a primitive Object (num, String, List, Map).
  factory FooGetDefaultResponse.deserialize(Object? src) {
    return _$FooGetDefaultResponseDeserialize(src);
  }
  static FooGetDefaultResponse? deserializeOrNull(Object? src) {
    if (src == null) {
      return null;
    }
    return FooGetDefaultResponse.deserialize(src);
  }
  /// Checks if a primitive Object (num, String, List, Map) can be deserialized.
  static bool canDeserialize(Object? src) {
    return _$FooGetDefaultResponseCanDeserialize(src);
  }
  /// Serializes to a primitive Object (num, String, List, Map).
  Map<String,dynamic> serialize() {
    return _$FooGetDefaultResponseSerialize(this);
  }
}




