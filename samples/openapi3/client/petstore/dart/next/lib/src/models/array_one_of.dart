// Model def

import 'package:openapi/_internal.dart';


part 'array_one_of.reflection.dart';
part 'array_one_of.serialization.dart';


/// ArrayOneOfMixin
mixin ArrayOneOfMixin on 
  $OpenApiObjectMixin {
    
  UndefinedWrapper<
            int
> get oneOf0;
  UndefinedWrapper<
    List<
        
            String
>
> get oneOf1;
}

/// ArrayOneOf
class ArrayOneOf with
$OpenApiObjectMixin,


ArrayOneOfMixin {

  

  
  @override
  UndefinedWrapper<
            int
> oneOf0;
  
  @override
  UndefinedWrapper<
    List<
        
            String
>
> oneOf1;
  

  ArrayOneOf.$all({
        
    
    required this.oneOf0,
    required this.oneOf1,
  });

  ArrayOneOf({
        
    
    this.oneOf0 = const UndefinedWrapper.undefined(),
    this.oneOf1 = const UndefinedWrapper.undefined(),
  });

  static const $reflection = ArrayOneOfReflection.instance;

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
    return _$ArrayOneOfToMap(this);
  }
  factory ArrayOneOf.fromMap(Map<String, dynamic> src) {
    return _$ArrayOneOfFromMap(src);
  }
  static ArrayOneOf? fromMapOrNull(Map<String, dynamic>? src) {
    if (src == null) {
      return null;
    }
    return ArrayOneOf.fromMap(src);
  }
  static bool canFromMap(Map<String, dynamic>? src) {
    if (src  == null) {
      return false;
    }
    return _$ArrayOneOfCanFromMap(src);
  }


  /// Deserializes a primitive Object (num, String, List, Map).
  factory ArrayOneOf.deserialize(Object? src) {
    return _$ArrayOneOfDeserialize(src);
  }
  static ArrayOneOf? deserializeOrNull(Object? src) {
    if (src == null) {
      return null;
    }
    return ArrayOneOf.deserialize(src);
  }
  /// Checks if a primitive Object (num, String, List, Map) can be deserialized.
  static bool canDeserialize(Object? src) {
    return _$ArrayOneOfCanDeserialize(src);
  }
  /// Serializes to a primitive Object (num, String, List, Map).
  Object? serialize() {
    return _$ArrayOneOfSerialize(this);
  }
}




