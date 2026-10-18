//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

// ignore_for_file: unused_element
import 'package:freezed_annotation/freezed_annotation.dart';

part 'class_model.freezed.dart';
part 'class_model.g.dart';


@freezed
abstract class ClassModel with _$ClassModel {
  const factory ClassModel({
    @JsonKey(
      name: r'_class',
      required: false,
      includeIfNull: false,
    )
    String? class_,
  }) = _ClassModel;

  factory ClassModel.fromJson(Map<String, dynamic> json) =>
      _$ClassModelFromJson(json);
}

