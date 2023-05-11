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
import 'package:built_value/iso_8601_date_time_serializer.dart';

import 'package:openapi/models.dart';
part 'serializers.g.dart';

@SerializersFor([
  Addressable,$Addressable,
  Bar,
  BarCreate,
  BarRef,
  BarRefOrValue,
  Entity,$Entity,
  EntityRef,$EntityRef,
  Extensible,$Extensible,
  Foo,
  FooRef,
  FooRefOrValue,
  Pasta,
  Pizza,$Pizza,
  PizzaSpeziale,
])
Serializers serializers = (_$serializers.toBuilder()
      ..addBuilderFactory(
        const FullType(BuiltList, [FullType(FooRefOrValue)]),
        () => ListBuilder<FooRefOrValue>(),
      )
      ..add(Addressable.serializer)
      ..add(Entity.serializer)
      ..add(EntityRef.serializer)
      ..add(Extensible.serializer)
      ..add(Pizza.serializer)
      ..add(const OneOfSerializer())
      ..add(const AnyOfSerializer())
      ..add(const DateSerializer())
      ..add(Iso8601DateTimeSerializer()))
    .build();

Serializers standardSerializers =
    (serializers.toBuilder()..addPlugin(StandardJsonPlugin())).build();
