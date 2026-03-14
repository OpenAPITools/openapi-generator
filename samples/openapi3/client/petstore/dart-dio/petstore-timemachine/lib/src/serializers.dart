//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

// ignore_for_file: unused_import

import 'package:one_of_serializer/any_of_serializer.dart';
import 'package:one_of_serializer/one_of_serializer.dart';
import 'package:built_collection/built_collection.dart';
import 'package:built_value/json_object.dart';
import 'package:built_value/serializer.dart';
import 'package:built_value/standard_json_plugin.dart';

import 'package:time_machine/time_machine.dart';
import 'package:openapi/src/offset_date_serializer.dart';
import 'package:openapi/src/model/api_response.dart';
import 'package:openapi/src/model/category.dart';
import 'package:openapi/src/model/order.dart';
import 'package:openapi/src/model/pet.dart';
import 'package:openapi/src/model/tag.dart';
import 'package:openapi/src/model/user.dart';

part 'serializers.g.dart';

@SerializersFor([
  ApiResponse,
  Category,
  Order,
  Pet,
  Tag,
  User,
])
Serializers serializers = (_$serializers.toBuilder()
      ..addBuilderFactory(
        const FullType(BuiltList, [FullType(User)]),
        () => ListBuilder<User>(),
      )
      ..addBuilderFactory(
        const FullType(BuiltList, [FullType(Pet)]),
        () => ListBuilder<Pet>(),
      )
      ..addBuilderFactory(
        const FullType(BuiltMap, [FullType(String), FullType(int)]),
        () => MapBuilder<String, int>(),
      )
      ..addBuilderFactory(
        const FullType(BuiltList, [FullType(String)]),
        () => ListBuilder<String>(),
      )
      ..add(const OneOfSerializer())
      ..add(const AnyOfSerializer())      
      ..add(const OffsetDateSerializer())
      ..add(const OffsetDateTimeSerializer())
    ).build();

Serializers standardSerializers =
    (serializers.toBuilder()..addPlugin(StandardJsonPlugin())).build();
