// Model def

import 'package:petstore_api/_internal.dart';


part 'class_model.reflection.dart';
part 'class_model.serialization.dart';


/// Model for testing model with \"_class\" property
///
/// Properties:
/// * [propertyClass] 
mixin ClassModelMixin on
  $OpenApiObjectMixin {
  UndefinedWrapper<
            String
> get propertyClass;
  
}

/// Model for testing model with \"_class\" property
///
/// Properties:
/// * [propertyClass] 
class ClassModel with
$OpenApiObjectMixin,

ClassModelMixin {
  @override
  UndefinedWrapper<
            String
> propertyClass;

  AdditionalProperties<Object
?> additionalProperties;

  

  ClassModel.$all({
        required this.propertyClass,
    required this.additionalProperties,
    
  });

  ClassModel({
      this.propertyClass = const UndefinedWrapper
        .undefined()
,
    this.additionalProperties = const AdditionalProperties(),
    
  });

  static const $reflection = ClassModelReflection.instance;
  ClassModelReflection get $classReflection => $reflection;

  @override
  bool validate() {
    return super.validate();
  }

  Map<String, dynamic> toMap() {
    return _$ClassModelToMap(this);
  }
  factory ClassModel.fromMap(Map<String, dynamic> src) {
    return _$ClassModelFromMap(src);
  }
  static ClassModel? fromMapOrNull(Map<String, dynamic>? src) {
    if (src == null) {
      return null;
    }
    return ClassModel.fromMap(src);
  }
  static bool canFromMap(Map<String, dynamic>? src) {
    if (src  == null) {
      return false;
    }
    return _$ClassModelCanFromMap(src);
  }


  /// Deserializes a primitive Object (num, String, List, Map).
  factory ClassModel.deserialize(Object? src) {
    return _$ClassModelDeserialize(src);
  }
  static ClassModel? deserializeOrNull(Object? src) {
    if (src == null) {
      return null;
    }
    return ClassModel.deserialize(src);
  }
  /// Checks if a primitive Object (num, String, List, Map) can be deserialized.
  static bool canDeserialize(Object? src) {
    return _$ClassModelCanDeserialize(src);
  }
  /// Serializes to a primitive Object (num, String, List, Map).
  Map<String,dynamic> serialize() {
    return _$ClassModelSerialize(this);
  }
}




