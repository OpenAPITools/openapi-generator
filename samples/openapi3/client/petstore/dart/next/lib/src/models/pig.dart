// Model def

import 'package:openapi/_internal.dart';


part 'pig.reflection.dart';
part 'pig.serialization.dart';


/// PigMixin
///
/// Properties:
mixin PigMixin on 
  $OpenApiObjectMixin {
    
  UndefinedWrapper<
            BasquePig
> get oneOf0;
  UndefinedWrapper<
            DanishPig
> get oneOf1;
}

/// Pig
///
/// Properties:
class Pig with
$OpenApiObjectMixin,


PigMixin {

  

  
  @override
  UndefinedWrapper<
            BasquePig
> oneOf0;
  
  @override
  UndefinedWrapper<
            DanishPig
> oneOf1;
  

  Pig.$all({
        
    
    required this.oneOf0,
    required this.oneOf1,
  });

  Pig({
        
    
    this.oneOf0 = const UndefinedWrapper.undefined(),
    this.oneOf1 = const UndefinedWrapper.undefined(),
  });

  static const $reflection = PigReflection.instance;

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
    return _$PigToMap(this);
  }
  factory Pig.fromMap(Map<String, dynamic> src) {
    return _$PigFromMap(src);
  }
  static Pig? fromMapOrNull(Map<String, dynamic>? src) {
    if (src == null) {
      return null;
    }
    return Pig.fromMap(src);
  }
  static bool canFromMap(Map<String, dynamic>? src) {
    if (src  == null) {
      return false;
    }
    return _$PigCanFromMap(src);
  }


  /// Deserializes a primitive Object (num, String, List, Map).
  factory Pig.deserialize(Object? src) {
    return _$PigDeserialize(src);
  }
  static Pig? deserializeOrNull(Object? src) {
    if (src == null) {
      return null;
    }
    return Pig.deserialize(src);
  }
  /// Checks if a primitive Object (num, String, List, Map) can be deserialized.
  static bool canDeserialize(Object? src) {
    return _$PigCanDeserialize(src);
  }
  /// Serializes to a primitive Object (num, String, List, Map).
  Object? serialize() {
    return _$PigSerialize(this);
  }
}




