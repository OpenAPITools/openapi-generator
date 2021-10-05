//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//
// @dart=2.0

// ignore_for_file: unused_element, unused_import
// ignore_for_file: always_put_required_named_parameters_first
// ignore_for_file: constant_identifier_names
// ignore_for_file: lines_longer_than_80_chars

library openapi.api;

import 'dart:async';
import 'dart:convert';
import 'dart:io';

import 'package:http/http.dart';
import 'package:intl/intl.dart';
import 'package:meta/meta.dart';

part 'api_client.dart';
part 'api_helper.dart';
part 'api_exception.dart';
part 'auth/authentication.dart';
part 'auth/api_key_auth.dart';
part 'auth/oauth.dart';
part 'auth/http_basic_auth.dart';
part 'auth/http_bearer_auth.dart';

part 'api/another_fake_api.dart';
part 'api/default_api.dart';
part 'api/fake_api.dart';
part 'api/fake_classname_tags123_api.dart';
part 'api/pet_api.dart';
part 'api/store_api.dart';
part 'api/user_api.dart';

part 'model/additional_properties_class.dart';
part 'model/animal.dart';
part 'model/api_response.dart';
part 'model/array_of_array_of_number_only.dart';
part 'model/array_of_number_only.dart';
part 'model/array_test.dart';
part 'model/capitalization.dart';
part 'model/cat.dart';
part 'model/cat_all_of.dart';
part 'model/category.dart';
part 'model/class_model.dart';
part 'model/deprecated_object.dart';
part 'model/dog.dart';
part 'model/dog_all_of.dart';
part 'model/enum_arrays.dart';
part 'model/enum_class.dart';
part 'model/enum_test.dart';
part 'model/file_schema_test_class.dart';
part 'model/foo.dart';
part 'model/format_test.dart';
part 'model/has_only_read_only.dart';
part 'model/health_check_result.dart';
part 'model/inline_response_default.dart';
part 'model/map_test.dart';
part 'model/mixed_properties_and_additional_properties_class.dart';
part 'model/model200_response.dart';
part 'model/model_client.dart';
part 'model/model_file.dart';
part 'model/model_list.dart';
part 'model/model_return.dart';
part 'model/name.dart';
part 'model/nullable_class.dart';
part 'model/number_only.dart';
part 'model/object_with_deprecated_fields.dart';
part 'model/order.dart';
part 'model/outer_composite.dart';
part 'model/outer_enum.dart';
part 'model/outer_enum_default_value.dart';
part 'model/outer_enum_integer.dart';
part 'model/outer_enum_integer_default_value.dart';
part 'model/outer_object_with_enum_property.dart';
part 'model/pet.dart';
part 'model/read_only_first.dart';
part 'model/special_model_name.dart';
part 'model/tag.dart';
part 'model/user.dart';


const _delimiters = {'csv': ',', 'ssv': ' ', 'tsv': '\t', 'pipes': '|'};
const _dateEpochMarker = 'epoch';
final _dateFormatter = DateFormat('yyyy-MM-dd');
final _regList = RegExp(r'^List<(.*)>$');
final _regSet = RegExp(r'^Set<(.*)>$');
final _regMap = RegExp(r'^Map<String,(.*)>$');

ApiClient defaultApiClient = ApiClient();
