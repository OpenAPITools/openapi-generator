// Model def

import 'package:petstore_api/_internal.dart';


part 'all_of_model_array_any_of_all_of_link_list_column1.reflection.dart';
part 'all_of_model_array_any_of_all_of_link_list_column1.serialization.dart';


/// AllOfModelArrayAnyOfAllOfLinkListColumn1Mixin
///
/// Properties:
/// * [value] 
mixin AllOfModelArrayAnyOfAllOfLinkListColumn1Mixin on
  $OpenApiObjectMixin {
  
    List<
        
            AllOfModelArrayAnyOfAllOfLinkListColumn1Value
>
 get value;
  
}

/// AllOfModelArrayAnyOfAllOfLinkListColumn1
///
/// Properties:
/// * [value] 
class AllOfModelArrayAnyOfAllOfLinkListColumn1 with
$OpenApiObjectMixin,

AllOfModelArrayAnyOfAllOfLinkListColumn1Mixin {
  @override
  
    List<
        
            AllOfModelArrayAnyOfAllOfLinkListColumn1Value
>
 value;

  AdditionalProperties<Object
?> additionalProperties;

  

  AllOfModelArrayAnyOfAllOfLinkListColumn1.$all({
        required this.value,
    required this.additionalProperties,
    
  });

  AllOfModelArrayAnyOfAllOfLinkListColumn1({
    required  this.value     ,
    this.additionalProperties = const AdditionalProperties(),
    
  });

  static const $reflection = AllOfModelArrayAnyOfAllOfLinkListColumn1Reflection.instance;
  AllOfModelArrayAnyOfAllOfLinkListColumn1Reflection get $classReflection => $reflection;

  @override
  bool validate() {
    return super.validate();
  }

  Map<String, dynamic> toMap() {
    return _$AllOfModelArrayAnyOfAllOfLinkListColumn1ToMap(this);
  }
  factory AllOfModelArrayAnyOfAllOfLinkListColumn1.fromMap(Map<String, dynamic> src) {
    return _$AllOfModelArrayAnyOfAllOfLinkListColumn1FromMap(src);
  }
  static AllOfModelArrayAnyOfAllOfLinkListColumn1? fromMapOrNull(Map<String, dynamic>? src) {
    if (src == null) {
      return null;
    }
    return AllOfModelArrayAnyOfAllOfLinkListColumn1.fromMap(src);
  }
  static bool canFromMap(Map<String, dynamic>? src) {
    if (src  == null) {
      return false;
    }
    return _$AllOfModelArrayAnyOfAllOfLinkListColumn1CanFromMap(src);
  }


  /// Deserializes a primitive Object (num, String, List, Map).
  factory AllOfModelArrayAnyOfAllOfLinkListColumn1.deserialize(Object? src) {
    return _$AllOfModelArrayAnyOfAllOfLinkListColumn1Deserialize(src);
  }
  static AllOfModelArrayAnyOfAllOfLinkListColumn1? deserializeOrNull(Object? src) {
    if (src == null) {
      return null;
    }
    return AllOfModelArrayAnyOfAllOfLinkListColumn1.deserialize(src);
  }
  /// Checks if a primitive Object (num, String, List, Map) can be deserialized.
  static bool canDeserialize(Object? src) {
    return _$AllOfModelArrayAnyOfAllOfLinkListColumn1CanDeserialize(src);
  }
  /// Serializes to a primitive Object (num, String, List, Map).
  Map<String,dynamic> serialize() {
    return _$AllOfModelArrayAnyOfAllOfLinkListColumn1Serialize(this);
  }
}




