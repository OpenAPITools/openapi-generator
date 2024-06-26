// Model def

import 'package:openapi/_internal.dart';


part 'object_with_deprecated_fields.reflection.dart';
part 'object_with_deprecated_fields.serialization.dart';


/// ObjectWithDeprecatedFieldsMixin
///
/// Properties:
/// * [uuid] 
/// * [id] 
/// * [deprecatedRef] 
/// * [bars] 
mixin ObjectWithDeprecatedFieldsMixin on
  $OpenApiObjectMixin {
  UndefinedWrapper<
            String
> get uuid;
UndefinedWrapper<
            num
> get id;
UndefinedWrapper<
            DeprecatedObject
> get deprecatedRef;
UndefinedWrapper<
    List<
        
            String
>
> get bars;
  
}

/// ObjectWithDeprecatedFields
///
/// Properties:
/// * [uuid] 
/// * [id] 
/// * [deprecatedRef] 
/// * [bars] 
class ObjectWithDeprecatedFields with
$OpenApiObjectMixin,


ObjectWithDeprecatedFieldsMixin {
  @override
  UndefinedWrapper<
            String
> uuid;
  @override
  UndefinedWrapper<
            num
> id;
  @override
  UndefinedWrapper<
            DeprecatedObject
> deprecatedRef;
  @override
  UndefinedWrapper<
    List<
        
            String
>
> bars;

  AdditionalProperties<Object
?> additionalProperties;

  

  ObjectWithDeprecatedFields.$all({
        required this.uuid,
    required this.id,
    required this.deprecatedRef,
    required this.bars,
    required this.additionalProperties,
    
  });

  ObjectWithDeprecatedFields({
      this.uuid = const UndefinedWrapper
        .undefined()
,
  this.id = const UndefinedWrapper
        .undefined()
,
  this.deprecatedRef = const UndefinedWrapper
        .undefined()
,
  this.bars = const UndefinedWrapper
        .undefined()
,
    this.additionalProperties = const AdditionalProperties(),
    
  });

  static const $reflection = ObjectWithDeprecatedFieldsReflection.instance;

  @override
  bool validate() {
    return super.validate();
  }


  Map<String, dynamic> toMap() {
    return _$ObjectWithDeprecatedFieldsToMap(this);
  }
  factory ObjectWithDeprecatedFields.fromMap(Map<String, dynamic> src) {
    return _$ObjectWithDeprecatedFieldsFromMap(src);
  }
  static ObjectWithDeprecatedFields? fromMapOrNull(Map<String, dynamic>? src) {
    if (src == null) {
      return null;
    }
    return ObjectWithDeprecatedFields.fromMap(src);
  }
  static bool canFromMap(Map<String, dynamic>? src) {
    if (src  == null) {
      return false;
    }
    return _$ObjectWithDeprecatedFieldsCanFromMap(src);
  }


  /// Deserializes a primitive Object (num, String, List, Map).
  factory ObjectWithDeprecatedFields.deserialize(Object? src) {
    return _$ObjectWithDeprecatedFieldsDeserialize(src);
  }
  static ObjectWithDeprecatedFields? deserializeOrNull(Object? src) {
    if (src == null) {
      return null;
    }
    return ObjectWithDeprecatedFields.deserialize(src);
  }
  /// Checks if a primitive Object (num, String, List, Map) can be deserialized.
  static bool canDeserialize(Object? src) {
    return _$ObjectWithDeprecatedFieldsCanDeserialize(src);
  }
  /// Serializes to a primitive Object (num, String, List, Map).
  Object? serialize() {
    return _$ObjectWithDeprecatedFieldsSerialize(this);
  }
}




