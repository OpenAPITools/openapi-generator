library serializers;

import 'package:built_value/iso_8601_date_time_serializer.dart';
import 'package:built_value/serializer.dart';
import 'package:built_collection/built_collection.dart';
import 'package:built_value/json_object.dart';
import 'package:built_value/standard_json_plugin.dart';

import 'package:openapi/model/api_response.dart';
import 'package:openapi/model/category.dart';
import 'package:openapi/model/order.dart';
import 'package:openapi/model/pet.dart';
import 'package:openapi/model/tag.dart';
import 'package:openapi/model/update_pet_with_form_body.dart';
import 'package:openapi/model/upload_file_body.dart';
import 'package:openapi/model/user.dart';


part 'serializers.g.dart';

@SerializersFor(const [
ApiResponse,
Category,
Order,
Pet,
Tag,
UpdatePetWithFormBody,
UploadFileBody,
User,

])

//allow all models to be serialized within a list
Serializers serializers = (_$serializers.toBuilder()
..addBuilderFactory(
const FullType(BuiltList, const [const FullType(ApiResponse)]),
() => new ListBuilder<ApiResponse>())
..addBuilderFactory(
const FullType(BuiltList, const [const FullType(Category)]),
() => new ListBuilder<Category>())
..addBuilderFactory(
const FullType(BuiltList, const [const FullType(Order)]),
() => new ListBuilder<Order>())
..addBuilderFactory(
const FullType(BuiltList, const [const FullType(Pet)]),
() => new ListBuilder<Pet>())
..addBuilderFactory(
const FullType(BuiltList, const [const FullType(Tag)]),
() => new ListBuilder<Tag>())
..addBuilderFactory(
const FullType(BuiltList, const [const FullType(UpdatePetWithFormBody)]),
() => new ListBuilder<UpdatePetWithFormBody>())
..addBuilderFactory(
const FullType(BuiltList, const [const FullType(UploadFileBody)]),
() => new ListBuilder<UploadFileBody>())
..addBuilderFactory(
const FullType(BuiltList, const [const FullType(User)]),
() => new ListBuilder<User>())

..add(Iso8601DateTimeSerializer())
).build();

Serializers standardSerializers =
(serializers.toBuilder()
..addPlugin(StandardJsonPlugin())).build();
