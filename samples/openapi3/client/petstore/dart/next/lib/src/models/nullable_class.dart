// Model def

import 'package:petstore_api/_internal.dart';


part 'nullable_class.reflection.dart';


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
mixin NullableClassMixin on
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
        
            $FreeFormObject
>
?> get arrayNullableProp;
UndefinedWrapper<
    List<
        
            $FreeFormObject
?>
?> get arrayAndItemsNullableProp;
UndefinedWrapper<
    List<
        
            $FreeFormObject
?>
> get arrayItemsNullable;
UndefinedWrapper<
    Map<String, 
        
            $FreeFormObject
>
?> get objectNullableProp;
UndefinedWrapper<
    Map<String, 
        
            $FreeFormObject
?>
?> get objectAndItemsNullableProp;
UndefinedWrapper<
    Map<String, 
        
            $FreeFormObject
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
        
            $FreeFormObject
>
?> arrayNullableProp;
  @override
  UndefinedWrapper<
    List<
        
            $FreeFormObject
?>
?> arrayAndItemsNullableProp;
  @override
  UndefinedWrapper<
    List<
        
            $FreeFormObject
?>
> arrayItemsNullable;
  @override
  UndefinedWrapper<
    Map<String, 
        
            $FreeFormObject
>
?> objectNullableProp;
  @override
  UndefinedWrapper<
    Map<String, 
        
            $FreeFormObject
?>
?> objectAndItemsNullableProp;
  @override
  UndefinedWrapper<
    Map<String, 
        
            $FreeFormObject
?>
> objectItemsNullable;

  AdditionalProperties<
            $FreeFormObject
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
    AdditionalProperties<
            $FreeFormObject
?>? additionalProperties,
    
  }) : additionalProperties = additionalProperties ?? {};

  static const $reflection = NullableClassReflection.instance;
  NullableClassReflection get $classReflection => $reflection;

  @override
  bool validate() {
    return super.validate();
  }

  factory NullableClass.deserialize(Object? src, [SerializationContext context = const SerializationContext.json(),]) {
    return $reflection.deserialize(src, context);
  }

  static bool canDeserialize(Object? src, [SerializationContext context = const SerializationContext.json(),]) {
    return $reflection.canDeserialize(src, context);
  }

  Object? serialize([SerializationContext context = const SerializationContext.json(),]) {
    return $reflection.serialize(this, context);
  }

  NullableClass clone() {
    return $reflection.clone(this);
  }
}


















































