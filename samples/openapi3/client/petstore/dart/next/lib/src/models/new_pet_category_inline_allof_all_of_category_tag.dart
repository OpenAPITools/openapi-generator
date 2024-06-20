// Model def

import 'dart:convert';
import 'dart:typed_data';
import 'package:openapi/_internal.dart';
import 'package:xml/xml.dart';

part 'new_pet_category_inline_allof_all_of_category_tag.reflection.dart';
part 'new_pet_category_inline_allof_all_of_category_tag.serialization.dart';

//class defination

///
mixin NewPetCategoryInlineAllofAllOfCategoryTagMixin on $OpenApiObjectMixin {
  UndefinedWrapper<int> get id;
  UndefinedWrapper<String> get name;
}

///
class NewPetCategoryInlineAllofAllOfCategoryTag
    with $OpenApiObjectMixin, NewPetCategoryInlineAllofAllOfCategoryTagMixin {
  @override
  UndefinedWrapper<int> id;
  @override
  UndefinedWrapper<String> name;

  NewPetCategoryInlineAllofAllOfCategoryTag.$all({
    required this.id,
    required this.name,
  });

  NewPetCategoryInlineAllofAllOfCategoryTag({
    this.id = const UndefinedWrapper.undefined(),
    this.name = const UndefinedWrapper.undefined(),
  });
}
