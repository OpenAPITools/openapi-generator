// Model def

import 'dart:convert';
import 'dart:typed_data';
import 'package:openapi/_internal.dart';
import 'package:xml/xml.dart';

part '__200_response.reflection.dart';
part '__200_response.serialization.dart';

//class defination

///
mixin $200ResponseMixin on $OpenApiObjectMixin {
  UndefinedWrapper<int> get name;
  UndefinedWrapper<String> get propertyClass;
}

///
class $200Response with $OpenApiObjectMixin, $200ResponseMixin {
  @override
  UndefinedWrapper<int> name;
  @override
  UndefinedWrapper<String> propertyClass;

  $200Response.$all({
    required this.name,
    required this.propertyClass,
  });

  $200Response({
    this.name = const UndefinedWrapper.undefined(),
    this.propertyClass = const UndefinedWrapper.undefined(),
  });
}
