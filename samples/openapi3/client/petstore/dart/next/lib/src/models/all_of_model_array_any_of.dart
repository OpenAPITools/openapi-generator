// Model def

import 'package:openapi/_internal.dart';


part 'all_of_model_array_any_of.reflection.dart';
part 'all_of_model_array_any_of.serialization.dart';


/// AllOfModelArrayAnyOfMixin
///
/// Properties:
/// * [linkListColumn1] 
/// * [attributes] 
mixin AllOfModelArrayAnyOfMixin on
  CategoryMixin, $OpenApiObjectMixin {
  UndefinedWrapper<
            AllOfModelArrayAnyOfAllOfLinkListColumn1
> get linkListColumn1;
UndefinedWrapper<
            AllOfModelArrayAnyOfAllOfAttributes
> get attributes;
  
}

/// AllOfModelArrayAnyOf
///
/// Properties:
/// * [name] 
/// * [attributes] 
/// * [id] 
/// * [linkListColumn1] 
class AllOfModelArrayAnyOf with
$OpenApiObjectMixin,

CategoryMixin,
AllOfModelArrayAnyOfMixin {
  @override
  
            String
 name;
  @override
  UndefinedWrapper<
            AllOfModelArrayAnyOfAllOfAttributes
> attributes;
  @override
  UndefinedWrapper<
            int
> id;
  @override
  UndefinedWrapper<
            AllOfModelArrayAnyOfAllOfLinkListColumn1
> linkListColumn1;

  AdditionalProperties<Object
?> additionalProperties;

  

  AllOfModelArrayAnyOf.$all({
        required this.name,
    required this.attributes,
    required this.id,
    required this.linkListColumn1,
    required this.additionalProperties,
    
  });

  AllOfModelArrayAnyOf({
      this.name     =
        
        'default-name'
        
,
  this.attributes = const UndefinedWrapper
        .undefined()
,
  this.id = const UndefinedWrapper
        .undefined()
,
  this.linkListColumn1 = const UndefinedWrapper
        .undefined()
,
    this.additionalProperties = const AdditionalProperties(),
    
  });

  static const $reflection = AllOfModelArrayAnyOfReflection.instance;

  @override
  bool validate() {
      
      
    return super.validate();
  }


  Map<String, dynamic> toMap() {
    return _$AllOfModelArrayAnyOfToMap(this);
  }
  factory AllOfModelArrayAnyOf.fromMap(Map<String, dynamic> src) {
    return _$AllOfModelArrayAnyOfFromMap(src);
  }
  static AllOfModelArrayAnyOf? fromMapOrNull(Map<String, dynamic>? src) {
    if (src == null) {
      return null;
    }
    return AllOfModelArrayAnyOf.fromMap(src);
  }
  static bool canFromMap(Map<String, dynamic>? src) {
    if (src  == null) {
      return false;
    }
    return _$AllOfModelArrayAnyOfCanFromMap(src);
  }


  /// Deserializes a primitive Object (num, String, List, Map).
  factory AllOfModelArrayAnyOf.deserialize(Object? src) {
    return _$AllOfModelArrayAnyOfDeserialize(src);
  }
  static AllOfModelArrayAnyOf? deserializeOrNull(Object? src) {
    if (src == null) {
      return null;
    }
    return AllOfModelArrayAnyOf.deserialize(src);
  }
  /// Checks if a primitive Object (num, String, List, Map) can be deserialized.
  static bool canDeserialize(Object? src) {
    return _$AllOfModelArrayAnyOfCanDeserialize(src);
  }
  /// Serializes to a primitive Object (num, String, List, Map).
  Object? serialize() {
    return _$AllOfModelArrayAnyOfSerialize(this);
  }
}




