// Model def

import 'dart:convert';
import 'dart:typed_data';
import 'package:openapi/_internal.dart';
import 'package:xml/xml.dart';

part 'property_name_collision.reflection.dart';
part 'property_name_collision.serialization.dart';

//class defination

///
mixin PropertyNameCollisionMixin on $OpenApiObjectMixin {
  UndefinedWrapper<String> get $type;
  UndefinedWrapper<String> get type;
  UndefinedWrapper<String> get type$;
}

///
class PropertyNameCollision
    with $OpenApiObjectMixin, PropertyNameCollisionMixin {
  @override
  UndefinedWrapper<String> $type;
  @override
  UndefinedWrapper<String> type;
  @override
  UndefinedWrapper<String> type$;

  PropertyNameCollision.$all({
    required this.$type,
    required this.type,
    required this.type$,
  });

  PropertyNameCollision({
    this.$type = const UndefinedWrapper.undefined(),
    this.type = const UndefinedWrapper.undefined(),
    this.type$ = const UndefinedWrapper.undefined(),
  });
}
