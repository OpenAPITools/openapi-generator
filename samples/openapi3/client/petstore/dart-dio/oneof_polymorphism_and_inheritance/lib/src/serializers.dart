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
import 'package:openapi/src/date_serializer.dart';
import 'package:openapi/src/model/date.dart';

import 'package:openapi/src/model/addressable.dart';
import 'package:openapi/src/model/bar.dart';
import 'package:openapi/src/model/bar_create.dart';
import 'package:openapi/src/model/bar_ref.dart';
import 'package:openapi/src/model/bar_ref_or_value.dart';
import 'package:openapi/src/model/entity.dart';
import 'package:openapi/src/model/entity_ref.dart';
import 'package:openapi/src/model/extensible.dart';
import 'package:openapi/src/model/foo.dart';
import 'package:openapi/src/model/foo_ref.dart';
import 'package:openapi/src/model/foo_ref_or_value.dart';
import 'package:openapi/src/model/pasta.dart';
import 'package:openapi/src/model/pizza.dart';
import 'package:openapi/src/model/pizza_speziale.dart';

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
