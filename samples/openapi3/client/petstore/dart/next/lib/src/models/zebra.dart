// Model def

import 'dart:convert';
import 'dart:typed_data';
import 'package:openapi/_internal.dart';
import 'package:xml/xml.dart';

part 'zebra.reflection.dart';
part 'zebra.serialization.dart';


//class defination

///
mixin ZebraMixin on  AdditionalPropertiesMixin<Object?>,
  
  $OpenApiObjectMixin
 {
  UndefinedWrapper<TypeEnum> get type;
  String get className;


}

///
class Zebra with
$OpenApiObjectMixin,
AdditionalPropertiesMixin<Object?>,

ZebraMixin {
  @override
  UndefinedWrapper<TypeEnum> type;
  @override
  String className;

  @override
  AdditionalProperties<Object?> additionalProperties;



  Zebra.$all({
    required this.type,
    required this.className,
    required this.additionalProperties,
    
  });

  Zebra({
    this.type = const UndefinedWrapper.undefined(),
  required  this.className ,
    this.additionalProperties = const AdditionalProperties(),
    
  });
}




//inline enum def

extension type const TypeEnum._(String value) {
      const TypeEnum.plains() : this._(r'plains');
      const TypeEnum.mountain() : this._(r'mountain');
      const TypeEnum.grevys() : this._(r'grevys');

  /// Creates a [TypeEnum] enum from a value and safely checking if it exists.
  factory TypeEnum.$safe(String value) {
    final res = values.where((element) => element.value == value).firstOrNull;
    if (res == null) {
      throw 'Invalid enum value $value';
    }
    return res;
  }

  /// Creates a [TypeEnum] enum from a value without checking if it exists.
  const TypeEnum.$unsafe(String value) : this._(value);

  /// All possible values of the enum.
  static const List<TypeEnum> values = [
    TypeEnum.plains(),
    TypeEnum.mountain(),
    TypeEnum.grevys(),
    
  ];
}

