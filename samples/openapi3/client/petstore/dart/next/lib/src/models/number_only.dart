// Model def

import 'package:petstore_api/_internal.dart';


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

  AdditionalProperties<Object
?> additionalProperties;

  

  NumberOnly.$all({
        required this.justNumber,
    required this.additionalProperties,
    
  });

  NumberOnly({
      this.justNumber = const UndefinedWrapper
        .undefined()
,
    this.additionalProperties = const AdditionalProperties(),
    
  });

  static const $reflection = NumberOnlyReflection.instance;
  NumberOnlyReflection get $classReflection => $reflection;

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
  Map<String,dynamic> serialize() {
    return _$NumberOnlySerialize(this);
  }
}




