//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

// ignore_for_file: unused_element
import 'package:freezed_annotation/freezed_annotation.dart';

part 'model_list.freezed.dart';
part 'model_list.g.dart';


@freezed
abstract class ModelList with _$ModelList {
  const factory ModelList({
    @JsonKey(
      name: r'123-list',
      required: false,
      includeIfNull: false,
    )
    String? n123list,
  }) = _ModelList;

  factory ModelList.fromJson(Map<String, dynamic> json) =>
      _$ModelListFromJson(json);
}

