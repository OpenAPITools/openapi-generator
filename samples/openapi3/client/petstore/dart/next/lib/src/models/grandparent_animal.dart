// Model def

import 'dart:convert';
import 'dart:typed_data';
import 'package:openapi/_internal.dart';
import 'package:xml/xml.dart';

part 'grandparent_animal.reflection.dart';
part 'grandparent_animal.serialization.dart';

//class defination

///
mixin GrandparentAnimalMixin on $OpenApiObjectMixin {
  String get petType;
}

///
class GrandparentAnimal with $OpenApiObjectMixin, GrandparentAnimalMixin {
  @override
  String petType;

  GrandparentAnimal.$all({
    required this.petType,
  });

  GrandparentAnimal({
    required this.petType,
  });
}
