// Model def

import 'dart:convert';
import 'dart:typed_data';
import 'package:openapi/_internal.dart';
import 'package:xml/xml.dart';

part 'nullable_class.reflection.dart';
part 'nullable_class.serialization.dart';


//class defination

///
mixin NullableClassMixin on  AdditionalPropertiesMixin<$OpenApiObjectMixin?>,
  
  $OpenApiObjectMixin
 {
  UndefinedWrapper<int?> get integerProp;
  UndefinedWrapper<num?> get numberProp;
  UndefinedWrapper<bool?> get booleanProp;
  UndefinedWrapper<String?> get stringProp;
  UndefinedWrapper<DateTime?> get dateProp;
  UndefinedWrapper<DateTime?> get datetimeProp;
  UndefinedWrapper<List<$OpenApiObjectMixin>?> get arrayNullableProp;
  UndefinedWrapper<List<$OpenApiObjectMixin?>?> get arrayAndItemsNullableProp;
  UndefinedWrapper<List<$OpenApiObjectMixin?>> get arrayItemsNullable;
  UndefinedWrapper<Map<String, $OpenApiObjectMixin>?> get objectNullableProp;
  UndefinedWrapper<Map<String, $OpenApiObjectMixin?>?> get objectAndItemsNullableProp;
  UndefinedWrapper<Map<String, $OpenApiObjectMixin?>> get objectItemsNullable;


}

///
class NullableClass with
$OpenApiObjectMixin,
AdditionalPropertiesMixin<$OpenApiObjectMixin?>,

NullableClassMixin {
  @override
  UndefinedWrapper<int?> integerProp;
  @override
  UndefinedWrapper<num?> numberProp;
  @override
  UndefinedWrapper<bool?> booleanProp;
  @override
  UndefinedWrapper<String?> stringProp;
  @override
  UndefinedWrapper<DateTime?> dateProp;
  @override
  UndefinedWrapper<DateTime?> datetimeProp;
  @override
  UndefinedWrapper<List<$OpenApiObjectMixin>?> arrayNullableProp;
  @override
  UndefinedWrapper<List<$OpenApiObjectMixin?>?> arrayAndItemsNullableProp;
  @override
  UndefinedWrapper<List<$OpenApiObjectMixin?>> arrayItemsNullable;
  @override
  UndefinedWrapper<Map<String, $OpenApiObjectMixin>?> objectNullableProp;
  @override
  UndefinedWrapper<Map<String, $OpenApiObjectMixin?>?> objectAndItemsNullableProp;
  @override
  UndefinedWrapper<Map<String, $OpenApiObjectMixin?>> objectItemsNullable;

  @override
  AdditionalProperties<$OpenApiObjectMixin?> additionalProperties;



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
    this.integerProp = const UndefinedWrapper.undefined(),
    this.numberProp = const UndefinedWrapper.undefined(),
    this.booleanProp = const UndefinedWrapper.undefined(),
    this.stringProp = const UndefinedWrapper.undefined(),
    this.dateProp = const UndefinedWrapper.undefined(),
    this.datetimeProp = const UndefinedWrapper.undefined(),
    this.arrayNullableProp = const UndefinedWrapper.undefined(),
    this.arrayAndItemsNullableProp = const UndefinedWrapper.undefined(),
    this.arrayItemsNullable = const UndefinedWrapper.undefined(),
    this.objectNullableProp = const UndefinedWrapper.undefined(),
    this.objectAndItemsNullableProp = const UndefinedWrapper.undefined(),
    this.objectItemsNullable = const UndefinedWrapper.undefined(),
    this.additionalProperties = const AdditionalProperties(),
    
  });
}




