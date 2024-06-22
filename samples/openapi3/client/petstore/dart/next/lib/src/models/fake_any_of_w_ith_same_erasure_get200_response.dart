// Model def

import 'package:openapi/_internal.dart';


part 'fake_any_of_w_ith_same_erasure_get200_response.reflection.dart';
part 'fake_any_of_w_ith_same_erasure_get200_response.serialization.dart';


/// FakeAnyOfWIthSameErasureGet200ResponseMixin
mixin FakeAnyOfWIthSameErasureGet200ResponseMixin on 
  $OpenApiObjectMixin {
    
  UndefinedWrapper<
    List<
        
            String
>
> get anyOf0;
  UndefinedWrapper<
    List<
        
            int
>
> get anyOf1;
}

/// FakeAnyOfWIthSameErasureGet200Response
class FakeAnyOfWIthSameErasureGet200Response with
$OpenApiObjectMixin,


FakeAnyOfWIthSameErasureGet200ResponseMixin {

  

  
  @override
  UndefinedWrapper<
    List<
        
            String
>
> anyOf0;
  
  @override
  UndefinedWrapper<
    List<
        
            int
>
> anyOf1;
  

  FakeAnyOfWIthSameErasureGet200Response.$all({
        
    
    required this.anyOf0,
    required this.anyOf1,
  });

  FakeAnyOfWIthSameErasureGet200Response({
        
    
    this.anyOf0 = const UndefinedWrapper.undefined(),
    
    this.anyOf1 = const UndefinedWrapper.undefined(),
    
  });

  static const $reflection = FakeAnyOfWIthSameErasureGet200ResponseReflection.instance;

  @override
  bool validate() {
      
      final anyOfs = [anyOf0,anyOf1,].where((e) => e.isDefined).take(1);
      if (anyOfs.length == 0) {
        // there must be AT LEAST one "anyOf" schema.
        return false;
      }
      
    return super.validate();
  }


  Map<String, dynamic> toMap() {
    return _$FakeAnyOfWIthSameErasureGet200ResponseToMap(this);
  }
  factory FakeAnyOfWIthSameErasureGet200Response.fromMap(Map<String, dynamic> src) {
    return _$FakeAnyOfWIthSameErasureGet200ResponseFromMap(src);
  }
  static FakeAnyOfWIthSameErasureGet200Response? fromMapOrNull(Map<String, dynamic>? src) {
    if (src == null) {
      return null;
    }
    return FakeAnyOfWIthSameErasureGet200Response.fromMap(src);
  }
  static bool canFromMap(Map<String, dynamic>? src) {
    if (src  == null) {
      return false;
    }
    return _$FakeAnyOfWIthSameErasureGet200ResponseCanFromMap(src);
  }


  /// Deserializes a primitive Object (num, String, List, Map).
  factory FakeAnyOfWIthSameErasureGet200Response.deserialize(Object? src) {
    return _$FakeAnyOfWIthSameErasureGet200ResponseDeserialize(src);
  }
  static FakeAnyOfWIthSameErasureGet200Response? deserializeOrNull(Object? src) {
    if (src == null) {
      return null;
    }
    return FakeAnyOfWIthSameErasureGet200Response.deserialize(src);
  }
  /// Checks if a primitive Object (num, String, List, Map) can be deserialized.
  static bool canDeserialize(Object? src) {
    return _$FakeAnyOfWIthSameErasureGet200ResponseCanDeserialize(src);
  }
  /// Serializes to a primitive Object (num, String, List, Map).
  Object? serialize() {
    return _$FakeAnyOfWIthSameErasureGet200ResponseSerialize(this);
  }
}




