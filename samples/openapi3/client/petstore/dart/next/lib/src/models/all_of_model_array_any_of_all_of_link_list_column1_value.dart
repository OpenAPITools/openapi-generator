// Model def

import 'package:petstore_api/_internal.dart';


part 'all_of_model_array_any_of_all_of_link_list_column1_value.reflection.dart';
part 'all_of_model_array_any_of_all_of_link_list_column1_value.serialization.dart';


/// AllOfModelArrayAnyOfAllOfLinkListColumn1ValueMixin
///
/// Properties:
mixin AllOfModelArrayAnyOfAllOfLinkListColumn1ValueMixin on
  $OpenApiObjectMixin {
    
  UndefinedWrapper<
            User
> get anyOf0;
  UndefinedWrapper<
            Tag
> get anyOf1;
}

/// AllOfModelArrayAnyOfAllOfLinkListColumn1Value
///
/// Properties:
class AllOfModelArrayAnyOfAllOfLinkListColumn1Value with
$OpenApiObjectMixin,

AllOfModelArrayAnyOfAllOfLinkListColumn1ValueMixin {

  AdditionalProperties<Object
?> additionalProperties;

  
  @override
  UndefinedWrapper<
            User
> anyOf0;
  
  @override
  UndefinedWrapper<
            Tag
> anyOf1;
  

  AllOfModelArrayAnyOfAllOfLinkListColumn1Value.$all({
        required this.additionalProperties,
    
    required this.anyOf0,
    required this.anyOf1,
  });

  AllOfModelArrayAnyOfAllOfLinkListColumn1Value({
        this.additionalProperties = const AdditionalProperties(),
    
    this.anyOf0 = const UndefinedWrapper.undefined(),
    
    this.anyOf1 = const UndefinedWrapper.undefined(),
    
  });

  static const $reflection = AllOfModelArrayAnyOfAllOfLinkListColumn1ValueReflection.instance;
  AllOfModelArrayAnyOfAllOfLinkListColumn1ValueReflection get $classReflection => $reflection;

  @override
  bool validate() {
      
      final anyOfs = [anyOf0,anyOf1,].where((e) => e.isDefined).take(1);
      if (anyOfs.length == 0) {
        // there must be AT LEAST one "anyOf" schema.
        return false;
      }
      
    return super.validate();
  }

  Map<String, dynamic> toMap() {
    return _$AllOfModelArrayAnyOfAllOfLinkListColumn1ValueToMap(this);
  }
  factory AllOfModelArrayAnyOfAllOfLinkListColumn1Value.fromMap(Map<String, dynamic> src) {
    return _$AllOfModelArrayAnyOfAllOfLinkListColumn1ValueFromMap(src);
  }
  static AllOfModelArrayAnyOfAllOfLinkListColumn1Value? fromMapOrNull(Map<String, dynamic>? src) {
    if (src == null) {
      return null;
    }
    return AllOfModelArrayAnyOfAllOfLinkListColumn1Value.fromMap(src);
  }
  static bool canFromMap(Map<String, dynamic>? src) {
    if (src  == null) {
      return false;
    }
    return _$AllOfModelArrayAnyOfAllOfLinkListColumn1ValueCanFromMap(src);
  }


  /// Deserializes a primitive Object (num, String, List, Map).
  factory AllOfModelArrayAnyOfAllOfLinkListColumn1Value.deserialize(Object? src) {
    return _$AllOfModelArrayAnyOfAllOfLinkListColumn1ValueDeserialize(src);
  }
  static AllOfModelArrayAnyOfAllOfLinkListColumn1Value? deserializeOrNull(Object? src) {
    if (src == null) {
      return null;
    }
    return AllOfModelArrayAnyOfAllOfLinkListColumn1Value.deserialize(src);
  }
  /// Checks if a primitive Object (num, String, List, Map) can be deserialized.
  static bool canDeserialize(Object? src) {
    return _$AllOfModelArrayAnyOfAllOfLinkListColumn1ValueCanDeserialize(src);
  }
  /// Serializes to a primitive Object (num, String, List, Map).
  Object? serialize() {
    return _$AllOfModelArrayAnyOfAllOfLinkListColumn1ValueSerialize(this);
  }
}




