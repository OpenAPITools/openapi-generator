// Model def

import 'dart:convert';
import 'dart:typed_data';
import 'package:openapi/_internal.dart';
import 'package:xml/xml.dart';

part 'mixed_properties_and_additional_properties_class.reflection.dart';
part 'mixed_properties_and_additional_properties_class.serialization.dart';

//class defination

///
mixin MixedPropertiesAndAdditionalPropertiesClassMixin on $OpenApiObjectMixin {
  UndefinedWrapper<String> get uuid;
  UndefinedWrapper<DateTime> get dateTime;
  UndefinedWrapper<Map<String, Animal>> get map;
}

///
class MixedPropertiesAndAdditionalPropertiesClass
    with $OpenApiObjectMixin, MixedPropertiesAndAdditionalPropertiesClassMixin {
  @override
  UndefinedWrapper<String> uuid;
  @override
  UndefinedWrapper<DateTime> dateTime;
  @override
  UndefinedWrapper<Map<String, Animal>> map;

  MixedPropertiesAndAdditionalPropertiesClass.$all({
    required this.uuid,
    required this.dateTime,
    required this.map,
  });

  MixedPropertiesAndAdditionalPropertiesClass({
    this.uuid = const UndefinedWrapper.undefined(),
    this.dateTime = const UndefinedWrapper.undefined(),
    this.map = const UndefinedWrapper.undefined(),
  });
}
