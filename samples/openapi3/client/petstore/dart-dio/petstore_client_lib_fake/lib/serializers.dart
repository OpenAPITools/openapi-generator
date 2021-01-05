//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//
// @dart=2.6

// ignore_for_file: unused_import

library serializers;

import 'package:built_value/iso_8601_date_time_serializer.dart';
import 'package:built_value/serializer.dart';
import 'package:built_collection/built_collection.dart';
import 'package:built_value/json_object.dart';
import 'package:built_value/standard_json_plugin.dart';

import 'package:openapi/model/additional_properties_class.dart';
import 'package:openapi/model/animal.dart';
import 'package:openapi/model/api_response.dart';
import 'package:openapi/model/array_of_array_of_number_only.dart';
import 'package:openapi/model/array_of_number_only.dart';
import 'package:openapi/model/array_test.dart';
import 'package:openapi/model/capitalization.dart';
import 'package:openapi/model/cat.dart';
import 'package:openapi/model/cat_all_of.dart';
import 'package:openapi/model/category.dart';
import 'package:openapi/model/class_model.dart';
import 'package:openapi/model/dog.dart';
import 'package:openapi/model/dog_all_of.dart';
import 'package:openapi/model/enum_arrays.dart';
import 'package:openapi/model/enum_test.dart';
import 'package:openapi/model/file_schema_test_class.dart';
import 'package:openapi/model/foo.dart';
import 'package:openapi/model/format_test.dart';
import 'package:openapi/model/has_only_read_only.dart';
import 'package:openapi/model/health_check_result.dart';
import 'package:openapi/model/inline_response_default.dart';
import 'package:openapi/model/map_test.dart';
import 'package:openapi/model/mixed_properties_and_additional_properties_class.dart';
import 'package:openapi/model/model200_response.dart';
import 'package:openapi/model/model_client.dart';
import 'package:openapi/model/model_enum_class.dart';
import 'package:openapi/model/model_file.dart';
import 'package:openapi/model/model_list.dart';
import 'package:openapi/model/model_return.dart';
import 'package:openapi/model/name.dart';
import 'package:openapi/model/nullable_class.dart';
import 'package:openapi/model/number_only.dart';
import 'package:openapi/model/order.dart';
import 'package:openapi/model/outer_composite.dart';
import 'package:openapi/model/outer_enum.dart';
import 'package:openapi/model/outer_enum_default_value.dart';
import 'package:openapi/model/outer_enum_integer.dart';
import 'package:openapi/model/outer_enum_integer_default_value.dart';
import 'package:openapi/model/pet.dart';
import 'package:openapi/model/read_only_first.dart';
import 'package:openapi/model/special_model_name.dart';
import 'package:openapi/model/tag.dart';
import 'package:openapi/model/user.dart';


part 'serializers.g.dart';

@SerializersFor(const [
AdditionalPropertiesClass,
Animal,
ApiResponse,
ArrayOfArrayOfNumberOnly,
ArrayOfNumberOnly,
ArrayTest,
Capitalization,
Cat,
CatAllOf,
Category,
ClassModel,
Dog,
DogAllOf,
EnumArrays,
EnumTest,
FileSchemaTestClass,
Foo,
FormatTest,
HasOnlyReadOnly,
HealthCheckResult,
InlineResponseDefault,
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
Order,
OuterComposite,
OuterEnum,
OuterEnumDefaultValue,
OuterEnumInteger,
OuterEnumIntegerDefaultValue,
Pet,
ReadOnlyFirst,
SpecialModelName,
Tag,
User,

])

//allow all models to be serialized within a list
Serializers serializers = (_$serializers.toBuilder()
..addBuilderFactory(
const FullType(BuiltList, const [const FullType(AdditionalPropertiesClass)]),
() => new ListBuilder<AdditionalPropertiesClass>())
..addBuilderFactory(
const FullType(BuiltList, const [const FullType(Animal)]),
() => new ListBuilder<Animal>())
..addBuilderFactory(
const FullType(BuiltList, const [const FullType(ApiResponse)]),
() => new ListBuilder<ApiResponse>())
..addBuilderFactory(
const FullType(BuiltList, const [const FullType(ArrayOfArrayOfNumberOnly)]),
() => new ListBuilder<ArrayOfArrayOfNumberOnly>())
..addBuilderFactory(
const FullType(BuiltList, const [const FullType(ArrayOfNumberOnly)]),
() => new ListBuilder<ArrayOfNumberOnly>())
..addBuilderFactory(
const FullType(BuiltList, const [const FullType(ArrayTest)]),
() => new ListBuilder<ArrayTest>())
..addBuilderFactory(
const FullType(BuiltList, const [const FullType(Capitalization)]),
() => new ListBuilder<Capitalization>())
..addBuilderFactory(
const FullType(BuiltList, const [const FullType(Cat)]),
() => new ListBuilder<Cat>())
..addBuilderFactory(
const FullType(BuiltList, const [const FullType(CatAllOf)]),
() => new ListBuilder<CatAllOf>())
..addBuilderFactory(
const FullType(BuiltList, const [const FullType(Category)]),
() => new ListBuilder<Category>())
..addBuilderFactory(
const FullType(BuiltList, const [const FullType(ClassModel)]),
() => new ListBuilder<ClassModel>())
..addBuilderFactory(
const FullType(BuiltList, const [const FullType(Dog)]),
() => new ListBuilder<Dog>())
..addBuilderFactory(
const FullType(BuiltList, const [const FullType(DogAllOf)]),
() => new ListBuilder<DogAllOf>())
..addBuilderFactory(
const FullType(BuiltList, const [const FullType(EnumArrays)]),
() => new ListBuilder<EnumArrays>())
..addBuilderFactory(
const FullType(BuiltList, const [const FullType(EnumTest)]),
() => new ListBuilder<EnumTest>())
..addBuilderFactory(
const FullType(BuiltList, const [const FullType(FileSchemaTestClass)]),
() => new ListBuilder<FileSchemaTestClass>())
..addBuilderFactory(
const FullType(BuiltList, const [const FullType(Foo)]),
() => new ListBuilder<Foo>())
..addBuilderFactory(
const FullType(BuiltList, const [const FullType(FormatTest)]),
() => new ListBuilder<FormatTest>())
..addBuilderFactory(
const FullType(BuiltList, const [const FullType(HasOnlyReadOnly)]),
() => new ListBuilder<HasOnlyReadOnly>())
..addBuilderFactory(
const FullType(BuiltList, const [const FullType(HealthCheckResult)]),
() => new ListBuilder<HealthCheckResult>())
..addBuilderFactory(
const FullType(BuiltList, const [const FullType(InlineResponseDefault)]),
() => new ListBuilder<InlineResponseDefault>())
..addBuilderFactory(
const FullType(BuiltList, const [const FullType(MapTest)]),
() => new ListBuilder<MapTest>())
..addBuilderFactory(
const FullType(BuiltList, const [const FullType(MixedPropertiesAndAdditionalPropertiesClass)]),
() => new ListBuilder<MixedPropertiesAndAdditionalPropertiesClass>())
..addBuilderFactory(
const FullType(BuiltList, const [const FullType(Model200Response)]),
() => new ListBuilder<Model200Response>())
..addBuilderFactory(
const FullType(BuiltList, const [const FullType(ModelClient)]),
() => new ListBuilder<ModelClient>())
..addBuilderFactory(
const FullType(BuiltList, const [const FullType(ModelEnumClass)]),
() => new ListBuilder<ModelEnumClass>())
..addBuilderFactory(
const FullType(BuiltList, const [const FullType(ModelFile)]),
() => new ListBuilder<ModelFile>())
..addBuilderFactory(
const FullType(BuiltList, const [const FullType(ModelList)]),
() => new ListBuilder<ModelList>())
..addBuilderFactory(
const FullType(BuiltList, const [const FullType(ModelReturn)]),
() => new ListBuilder<ModelReturn>())
..addBuilderFactory(
const FullType(BuiltList, const [const FullType(Name)]),
() => new ListBuilder<Name>())
..addBuilderFactory(
const FullType(BuiltList, const [const FullType(NullableClass)]),
() => new ListBuilder<NullableClass>())
..addBuilderFactory(
const FullType(BuiltList, const [const FullType(NumberOnly)]),
() => new ListBuilder<NumberOnly>())
..addBuilderFactory(
const FullType(BuiltList, const [const FullType(Order)]),
() => new ListBuilder<Order>())
..addBuilderFactory(
const FullType(BuiltList, const [const FullType(OuterComposite)]),
() => new ListBuilder<OuterComposite>())
..addBuilderFactory(
const FullType(BuiltList, const [const FullType(OuterEnum)]),
() => new ListBuilder<OuterEnum>())
..addBuilderFactory(
const FullType(BuiltList, const [const FullType(OuterEnumDefaultValue)]),
() => new ListBuilder<OuterEnumDefaultValue>())
..addBuilderFactory(
const FullType(BuiltList, const [const FullType(OuterEnumInteger)]),
() => new ListBuilder<OuterEnumInteger>())
..addBuilderFactory(
const FullType(BuiltList, const [const FullType(OuterEnumIntegerDefaultValue)]),
() => new ListBuilder<OuterEnumIntegerDefaultValue>())
..addBuilderFactory(
const FullType(BuiltList, const [const FullType(Pet)]),
() => new ListBuilder<Pet>())
..addBuilderFactory(
const FullType(BuiltList, const [const FullType(ReadOnlyFirst)]),
() => new ListBuilder<ReadOnlyFirst>())
..addBuilderFactory(
const FullType(BuiltList, const [const FullType(SpecialModelName)]),
() => new ListBuilder<SpecialModelName>())
..addBuilderFactory(
const FullType(BuiltList, const [const FullType(Tag)]),
() => new ListBuilder<Tag>())
..addBuilderFactory(
const FullType(BuiltList, const [const FullType(User)]),
() => new ListBuilder<User>())

..add(Iso8601DateTimeSerializer())
).build();

Serializers standardSerializers =
(serializers.toBuilder()
..addPlugin(StandardJsonPlugin())).build();
