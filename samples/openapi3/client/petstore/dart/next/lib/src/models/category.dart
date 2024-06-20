// Model def

import 'dart:convert';
import 'dart:typed_data';
import 'package:openapi/_internal.dart';
import 'package:xml/xml.dart';

part 'category.reflection.dart';
part 'category.serialization.dart';

//class defination

///
mixin CategoryMixin on $OpenApiObjectMixin {
  UndefinedWrapper<int> get id;
  String get name;
}

///
class Category with $OpenApiObjectMixin, CategoryMixin {
  @override
  UndefinedWrapper<int> id;
  @override
  String name;

  Category.$all({
    required this.id,
    required this.name,
  });

  Category({
    this.id = const UndefinedWrapper.undefined(),
    this.name = 'default-name',
  });
}
