// Model def

import 'package:openapi/_internal.dart';


part 'nullable_class.reflection.dart';
part 'nullable_class.serialization.dart';


/// NullableClassMixin
///
/// Properties:
/// * [integerProp] 
/// * [numberProp] 
/// * [booleanProp] 
/// * [stringProp] 
/// * [dateProp] 
/// * [datetimeProp] 
/// * [arrayNullableProp] 
/// * [arrayAndItemsNullableProp] 
/// * [arrayItemsNullable] 
/// * [objectNullableProp] 
/// * [objectAndItemsNullableProp] 
/// * [objectItemsNullable] 
mixin NullableClassMixin on  AdditionalPropertiesMixin<
            Map<String, Object?>
?>,
  $OpenApiObjectMixin {
  UndefinedWrapper<
            int
?> get integerProp;
UndefinedWrapper<
            num
?> get numberProp;
UndefinedWrapper<
            bool
?> get booleanProp;
UndefinedWrapper<
            String
?> get stringProp;
UndefinedWrapper<
            DateTime
?> get dateProp;
UndefinedWrapper<
            DateTime
?> get datetimeProp;
UndefinedWrapper<
    List<
        
            Map<String, Object?>
>
?> get arrayNullableProp;
UndefinedWrapper<
    List<
        
            Map<String, Object?>
?>
?> get arrayAndItemsNullableProp;
UndefinedWrapper<
    List<
        
            Map<String, Object?>
?>
> get arrayItemsNullable;
UndefinedWrapper<
    Map<String, 
        
            Map<String, Object?>
>
?> get objectNullableProp;
UndefinedWrapper<
    Map<String, 
        
            Map<String, Object?>
?>
?> get objectAndItemsNullableProp;
UndefinedWrapper<
    Map<String, 
        
            Map<String, Object?>
?>
> get objectItemsNullable;
  
}

/// NullableClass
///
/// Properties:
/// * [integerProp] 
/// * [numberProp] 
/// * [booleanProp] 
/// * [stringProp] 
/// * [dateProp] 
/// * [datetimeProp] 
/// * [arrayNullableProp] 
/// * [arrayAndItemsNullableProp] 
/// * [arrayItemsNullable] 
/// * [objectNullableProp] 
/// * [objectAndItemsNullableProp] 
/// * [objectItemsNullable] 
class NullableClass with
$OpenApiObjectMixin,
AdditionalPropertiesMixin<
            Map<String, Object?>
?>,

NullableClassMixin {
  @override
  UndefinedWrapper<
            int
?> integerProp;
  @override
  UndefinedWrapper<
            num
?> numberProp;
  @override
  UndefinedWrapper<
            bool
?> booleanProp;
  @override
  UndefinedWrapper<
            String
?> stringProp;
  @override
  UndefinedWrapper<
            DateTime
?> dateProp;
  @override
  UndefinedWrapper<
            DateTime
?> datetimeProp;
  @override
  UndefinedWrapper<
    List<
        
            Map<String, Object?>
>
?> arrayNullableProp;
  @override
  UndefinedWrapper<
    List<
        
            Map<String, Object?>
?>
?> arrayAndItemsNullableProp;
  @override
  UndefinedWrapper<
    List<
        
            Map<String, Object?>
?>
> arrayItemsNullable;
  @override
  UndefinedWrapper<
    Map<String, 
        
            Map<String, Object?>
>
?> objectNullableProp;
  @override
  UndefinedWrapper<
    Map<String, 
        
            Map<String, Object?>
?>
?> objectAndItemsNullableProp;
  @override
  UndefinedWrapper<
    Map<String, 
        
            Map<String, Object?>
?>
> objectItemsNullable;

  @override
  AdditionalProperties<
            Map<String, Object?>
?> additionalProperties;

  

  NullableClass.$all({
        required this.integerProp,
    required this.numberProp,
    required this.booleanProp,
    required this.stringProp,
    required this.dateProp,
    required this.datetimeProp,
    required this.arrayNullableProp,
    required this.arrayAndItemsNullableProp,
    required this.arrayItemsNullable,
    required this.objectNullableProp,
    required this.objectAndItemsNullableProp,
    required this.objectItemsNullable,
    required this.additionalProperties,
    
  });

  NullableClass({
      this.integerProp = const UndefinedWrapper
        .undefined()
,
  this.numberProp = const UndefinedWrapper
        .undefined()
,
  this.booleanProp = const UndefinedWrapper
        .undefined()
,
  this.stringProp = const UndefinedWrapper
        .undefined()
,
  this.dateProp = const UndefinedWrapper
        .undefined()
,
  this.datetimeProp = const UndefinedWrapper
        .undefined()
,
  this.arrayNullableProp = const UndefinedWrapper
        .undefined()
,
  this.arrayAndItemsNullableProp = const UndefinedWrapper
        .undefined()
,
  this.arrayItemsNullable = const UndefinedWrapper
        .undefined()
,
  this.objectNullableProp = const UndefinedWrapper
        .undefined()
,
  this.objectAndItemsNullableProp = const UndefinedWrapper
        .undefined()
,
  this.objectItemsNullable = const UndefinedWrapper
        .undefined()
,
    this.additionalProperties = const AdditionalProperties(),
    
  });

  static const $reflection = NullableClassReflection.instance;

  @override
  bool validate() {
    return super.validate();
  }


  Map<String, dynamic> toMap() {
    return _$NullableClassToMap(this);
  }
  factory NullableClass.fromMap(Map<String, dynamic> src) {
    return _$NullableClassFromMap(src);
  }
  static NullableClass? fromMapOrNull(Map<String, dynamic>? src) {
    if (src == null) {
      return null;
    }
    return NullableClass.fromMap(src);
  }
  static bool canFromMap(Map<String, dynamic>? src) {
    if (src  == null) {
      return false;
    }
    return _$NullableClassCanFromMap(src);
  }


  /// Deserializes a primitive Object (num, String, List, Map).
  factory NullableClass.deserialize(Object? src) {
    return _$NullableClassDeserialize(src);
  }
  static NullableClass? deserializeOrNull(Object? src) {
    if (src == null) {
      return null;
    }
    return NullableClass.deserialize(src);
  }
  /// Checks if a primitive Object (num, String, List, Map) can be deserialized.
  static bool canDeserialize(Object? src) {
    return _$NullableClassCanDeserialize(src);
  }
  /// Serializes to a primitive Object (num, String, List, Map).
  Object? serialize() {
    return _$NullableClassSerialize(this);
  }
}




