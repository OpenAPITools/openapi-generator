// Model def

import 'package:petstore_api/_internal.dart';


part 'tag.reflection.dart';
part 'tag.serialization.dart';


/// TagMixin
///
/// Properties:
/// * [id] 
/// * [name] 
mixin TagMixin on
  $OpenApiObjectMixin {
  UndefinedWrapper<
            int

> get id;
UndefinedWrapper<
            String

> get name;
  
}

/// Tag
///
/// Properties:
/// * [id] 
/// * [name] 
class Tag with
$OpenApiObjectMixin,

TagMixin {
  @override
  UndefinedWrapper<
            int

> id;
  @override
  UndefinedWrapper<
            String

> name;

  AdditionalProperties<Object

?> additionalProperties;

  

  Tag.$all({
        required this.id,
    required this.name,
    required this.additionalProperties,
    
  });

  Tag({
      this.id = const UndefinedWrapper
        .undefined()
,
  this.name = const UndefinedWrapper
        .undefined()
,
    this.additionalProperties = const AdditionalProperties(),
    
  });

  static const $reflection = TagReflection.instance;
  TagReflection get $classReflection => $reflection;

  @override
  bool validate() {
    return super.validate();
  }

  Map<String, dynamic> toMap() {
    return _$TagToMap(this);
  }
  factory Tag.fromMap(Map<String, dynamic> src) {
    return _$TagFromMap(src);
  }
  static Tag? fromMapOrNull(Map<String, dynamic>? src) {
    if (src == null) {
      return null;
    }
    return Tag.fromMap(src);
  }
  static bool canFromMap(Map<String, dynamic>? src) {
    if (src  == null) {
      return false;
    }
    return _$TagCanFromMap(src);
  }


  /// Deserializes a primitive Object (num, String, List, Map).
  factory Tag.deserialize(Object? src) {
    return _$TagDeserialize(src);
  }
  static Tag? deserializeOrNull(Object? src) {
    if (src == null) {
      return null;
    }
    return Tag.deserialize(src);
  }
  /// Checks if a primitive Object (num, String, List, Map) can be deserialized.
  static bool canDeserialize(Object? src) {
    return _$TagCanDeserialize(src);
  }
  /// Serializes to a primitive Object (num, String, List, Map).
  Map<String,dynamic> serialize() {
    return _$TagSerialize(this);
  }
}




