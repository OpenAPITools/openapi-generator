// Model def

import 'package:openapi/_internal.dart';


part 'number_only.reflection.dart';
part 'number_only.serialization.dart';


/// NumberOnlyMixin
///
/// Properties:
/// * [justNumber] 
mixin NumberOnlyMixin on 
  $OpenApiObjectMixin {
  UndefinedWrapper<
            num
> get justNumber;
  
}

/// NumberOnly
///
/// Properties:
/// * [justNumber] 
class NumberOnly with
$OpenApiObjectMixin,


NumberOnlyMixin {
  @override
  UndefinedWrapper<
            num
> justNumber;

  

  

  NumberOnly.$all({
        required this.justNumber,
    
    
  });

  NumberOnly({
      this.justNumber = const UndefinedWrapper
        .undefined()
,
    
    
  });

  static const $reflection = NumberOnlyReflection.instance;

  @override
  bool validate() {
    return super.validate();
  }


  Map<String, dynamic> toMap() {
    return _$NumberOnlyToMap(this);
  }
  factory NumberOnly.fromMap(Map<String, dynamic> src) {
    return _$NumberOnlyFromMap(src);
  }
  static NumberOnly? fromMapOrNull(Map<String, dynamic>? src) {
    if (src == null) {
      return null;
    }
    return NumberOnly.fromMap(src);
  }
  static bool canFromMap(Map<String, dynamic>? src) {
    if (src  == null) {
      return false;
    }
    return _$NumberOnlyCanFromMap(src);
  }


  /// Deserializes a primitive Object (num, String, List, Map).
  factory NumberOnly.deserialize(Object? src) {
    return _$NumberOnlyDeserialize(src);
  }
  static NumberOnly? deserializeOrNull(Object? src) {
    if (src == null) {
      return null;
    }
    return NumberOnly.deserialize(src);
  }
  /// Checks if a primitive Object (num, String, List, Map) can be deserialized.
  static bool canDeserialize(Object? src) {
    return _$NumberOnlyCanDeserialize(src);
  }
  /// Serializes to a primitive Object (num, String, List, Map).
  Object? serialize() {
    return _$NumberOnlySerialize(this);
  }
}




