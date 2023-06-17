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
import 'package:openapi/apis.dart';
import 'package:built_value/iso_8601_date_time_serializer.dart';

import 'package:openapi/models.dart';
part 'serializers.g.dart';

@SerializersFor([
  AdditionalPropertiesClass,
  Addressable,
  $Addressable,
  AllOfWithSingleRef,
  Animal,
  $Animal,
  ApiResponse,
  Apple,
  AppleAllOfDisc,
  AppleGrandparentDisc,
  AppleOneOfDisc,
  AppleReqDisc,
  AppleVariant1,
  ArrayOfArrayOfNumberOnly,
  ArrayOfNumberOnly,
  ArrayTest,
  Banana,
  BananaAllOfDisc,
  BananaGrandparentDisc,
  BananaOneOfDisc,
  BananaReqDisc,
  Bar,
  BarCreate,
  BarRef,
  BarRefOrValue,
  Capitalization,
  Cat,
  Category,
  ClassModel,
  ComposedDiscMissingFromProperties,
  ComposedDiscOptionalTypeCorrect,
  ComposedDiscOptionalTypeInconsistent,
  ComposedDiscOptionalTypeIncorrect,
  ComposedDiscRequiredInconsistent,
  ComposedDiscTypeInconsistent,
  ComposedDiscTypeIncorrect,
  DeprecatedObject,
  DiscMissingFromProperties,
  DiscOptionalTypeCorrect,
  DiscOptionalTypeIncorrect,
  DiscTypeIncorrect,
  Dog,
  Entity,
  $Entity,
  EntityRef,
  $EntityRef,
  EnumArrays,
  EnumTest,
  Extensible,
  $Extensible,
  FileSchemaTestClass,
  Foo,
  FooBasicGetDefaultResponse,
  FooRef,
  FooRefOrValue,
  FormatTest,
  Fruit,
  FruitAllOfDisc,
  FruitAnyOfDisc,
  FruitGrandparentDisc,
  FruitInlineDisc,
  FruitInlineDiscOneOf,
  FruitInlineDiscOneOf1,
  FruitInlineInlineDisc,
  FruitInlineInlineDiscOneOf,
  FruitInlineInlineDiscOneOf1,
  FruitInlineInlineDiscOneOfOneOf,
  FruitOneOfDisc,
  FruitReqDisc,
  FruitType,
  $FruitType,
  FruitVariant1,
  GigaOneOf,
  GrapeVariant1,
  HasOnlyReadOnly,
  HealthCheckResult,
  MapTest,
  MixedPropertiesAndAdditionalPropertiesClass,
  Model200Response,
  ModelClient,
  ModelEnumClass,
  ModelFile,
  ModelList,
  ModelReturn,
  Name,
  NullableClass,
  NumberOnly,
  ObjectWithDeprecatedFields,
  OneOfPrimitiveChild,
  Order,
  OuterComposite,
  OuterEnum,
  OuterEnumDefaultValue,
  OuterEnumInteger,
  OuterEnumIntegerDefaultValue,
  OuterObjectWithEnumProperty,
  Parent,
  $Parent,
  Pasta,
  Pet,
  Pizza,
  $Pizza,
  PizzaSpeziale,
  ReadOnlyFirst,
  SingleRefType,
  SpecialModelName,
  Tag,
  User,
])
Serializers serializers = (_$serializers.toBuilder()
      ..addBuilderFactory(
        const FullType(BuiltList, [FullType(EnumQueryStringArrayEnum)]),
        () => ListBuilder<EnumQueryStringArrayEnum>,
      )
      ..addBuilderFactory(
        const FullType(BuiltList, [FullType(EnumHeaderStringArrayEnum)]),
        () => ListBuilder<EnumHeaderStringArrayEnum>,
      )
      ..addBuilderFactory(
        const FullType(BuiltList, [FullType(InnerEnum)]),
        () => ListBuilder<InnerEnum>,
      )
      ..addBuilderFactory(
        const FullType(BuiltMap, [FullType(String), FullType(String)]),
        () => MapBuilder<String, String>,
      )
      ..addBuilderFactory(
        const FullType(BuiltList, [FullType(ModelEnumClass)]),
        () => ListBuilder<ModelEnumClass>,
      )
      ..addBuilderFactory(
        const FullType(BuiltList, [FullType(FooRefOrValue)]),
        () => ListBuilder<FooRefOrValue>,
      )
      ..addBuilderFactory(
        const FullType(BuiltList, [FullType(StatusEnum)]),
        () => ListBuilder<StatusEnum>,
      )
      ..addBuilderFactory(
        const FullType(BuiltMap, [FullType(String), FullType(int)]),
        () => MapBuilder<String, int>,
      )
      ..addBuilderFactory(
        const FullType(BuiltSet, [FullType(String)]),
        () => SetBuilder<String>,
      )
      ..addBuilderFactory(
        const FullType(BuiltList, [FullType(User)]),
        () => ListBuilder<User>,
      )
      ..addBuilderFactory(
        const FullType(BuiltList, [FullType(Pet)]),
        () => ListBuilder<Pet>,
      )
      ..addBuilderFactory(
        const FullType(BuiltList, [FullType(String)]),
        () => ListBuilder<String>,
      )
      ..addBuilderFactory(
        const FullType(BuiltSet, [FullType(Pet)]),
        () => SetBuilder<Pet>,
      )
      ..addBuilderFactory(
        const FullType(BuiltMap, [FullType(String), FullType(String)]),
        () => MapBuilder<String, String>,
      )
      ..add(Addressable.serializer)
      ..add(Animal.serializer)
      ..add(Entity.serializer)
      ..add(EntityRef.serializer)
      ..add(Extensible.serializer)
      ..add(FruitType.serializer)
      ..add(Parent.serializer)
      ..add(Pizza.serializer)
      ..add(const OneOfSerializer())
      ..add(const AnyOfSerializer())
      ..add(const DateSerializer())
      ..add(Iso8601DateTimeSerializer()))
    .build();

Serializers standardSerializers =
    (serializers.toBuilder()..addPlugin(StandardJsonPlugin())).build();
