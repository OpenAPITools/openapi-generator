// Model def

import 'package:openapi/_internal.dart';


part 'fake_one_of_w_ith_same_erasure_get200_response.reflection.dart';
part 'fake_one_of_w_ith_same_erasure_get200_response.serialization.dart';


/// FakeOneOfWIthSameErasureGet200ResponseMixin
mixin FakeOneOfWIthSameErasureGet200ResponseMixin on
  $OpenApiObjectMixin {
    
  UndefinedWrapper<
    List<
        
            String
>
> get oneOf0;
  UndefinedWrapper<
    List<
        
            int
>
> get oneOf1;
}

/// FakeOneOfWIthSameErasureGet200Response
class FakeOneOfWIthSameErasureGet200Response with
$OpenApiObjectMixin,


FakeOneOfWIthSameErasureGet200ResponseMixin {

  AdditionalProperties<Object
?> additionalProperties;

  
  @override
  UndefinedWrapper<
    List<
        
            String
>
> oneOf0;
  
  @override
  UndefinedWrapper<
    List<
        
            int
>
> oneOf1;
  

  FakeOneOfWIthSameErasureGet200Response.$all({
        required this.additionalProperties,
    
    required this.oneOf0,
    required this.oneOf1,
  });

  FakeOneOfWIthSameErasureGet200Response({
        this.additionalProperties = const AdditionalProperties(),
    
    this.oneOf0 = const UndefinedWrapper.undefined(),
    this.oneOf1 = const UndefinedWrapper.undefined(),
  });

  static const $reflection = FakeOneOfWIthSameErasureGet200ResponseReflection.instance;

  @override
  bool validate() {
      final oneOfs = [oneOf0,oneOf1,].where((e) => e.isDefined).take(2);
      if (oneOfs.length != 1) {
        // there must be EXACTLY one "oneOf" schema.
        return false;
      }
      
      
    return super.validate();
  }


  Map<String, dynamic> toMap() {
    return _$FakeOneOfWIthSameErasureGet200ResponseToMap(this);
  }
  factory FakeOneOfWIthSameErasureGet200Response.fromMap(Map<String, dynamic> src) {
    return _$FakeOneOfWIthSameErasureGet200ResponseFromMap(src);
  }
  static FakeOneOfWIthSameErasureGet200Response? fromMapOrNull(Map<String, dynamic>? src) {
    if (src == null) {
      return null;
    }
    return FakeOneOfWIthSameErasureGet200Response.fromMap(src);
  }
  static bool canFromMap(Map<String, dynamic>? src) {
    if (src  == null) {
      return false;
    }
    return _$FakeOneOfWIthSameErasureGet200ResponseCanFromMap(src);
  }


  /// Deserializes a primitive Object (num, String, List, Map).
  factory FakeOneOfWIthSameErasureGet200Response.deserialize(Object? src) {
    return _$FakeOneOfWIthSameErasureGet200ResponseDeserialize(src);
  }
  static FakeOneOfWIthSameErasureGet200Response? deserializeOrNull(Object? src) {
    if (src == null) {
      return null;
    }
    return FakeOneOfWIthSameErasureGet200Response.deserialize(src);
  }
  /// Checks if a primitive Object (num, String, List, Map) can be deserialized.
  static bool canDeserialize(Object? src) {
    return _$FakeOneOfWIthSameErasureGet200ResponseCanDeserialize(src);
  }
  /// Serializes to a primitive Object (num, String, List, Map).
  Object? serialize() {
    return _$FakeOneOfWIthSameErasureGet200ResponseSerialize(this);
  }
}




