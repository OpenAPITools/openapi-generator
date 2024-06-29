// Model def

import 'package:openapi/_internal.dart';


part 'category.reflection.dart';
part 'category.serialization.dart';


/// CategoryMixin
///
/// Properties:
/// * [id] 
/// * [name] 
mixin CategoryMixin on
  $OpenApiObjectMixin {
  UndefinedWrapper<
            int
> get id;

            String
 get name;
  
}

/// Category
///
/// Properties:
/// * [id] 
/// * [name] 
class Category with
$OpenApiObjectMixin,


CategoryMixin {
  @override
  UndefinedWrapper<
            int
> id;
  @override
  
            String
 name;

  AdditionalProperties<Object
?> additionalProperties;

  

  Category.$all({
        required this.id,
    required this.name,
    required this.additionalProperties,
    
  });

  Category({
      this.id = const UndefinedWrapper
        .undefined()
,
  this.name     =
        
        'default-name'
        
,
    this.additionalProperties = const AdditionalProperties(),
    
  });

  static const $reflection = CategoryReflection.instance;

  @override
  bool validate() {
    return super.validate();
  }


  Map<String, dynamic> toMap() {
    return _$CategoryToMap(this);
  }
  factory Category.fromMap(Map<String, dynamic> src) {
    return _$CategoryFromMap(src);
  }
  static Category? fromMapOrNull(Map<String, dynamic>? src) {
    if (src == null) {
      return null;
    }
    return Category.fromMap(src);
  }
  static bool canFromMap(Map<String, dynamic>? src) {
    if (src  == null) {
      return false;
    }
    return _$CategoryCanFromMap(src);
  }


  /// Deserializes a primitive Object (num, String, List, Map).
  factory Category.deserialize(Object? src) {
    return _$CategoryDeserialize(src);
  }
  static Category? deserializeOrNull(Object? src) {
    if (src == null) {
      return null;
    }
    return Category.deserialize(src);
  }
  /// Checks if a primitive Object (num, String, List, Map) can be deserialized.
  static bool canDeserialize(Object? src) {
    return _$CategoryCanDeserialize(src);
  }
  /// Serializes to a primitive Object (num, String, List, Map).
  Map<String,dynamic> serialize() {
    return _$CategorySerialize(this);
  }
}




