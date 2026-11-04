//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

// ignore_for_file: unused_element
import 'package:freezed_annotation/freezed_annotation.dart';

part 'model_return.freezed.dart';
part 'model_return.g.dart';


@freezed
abstract class ModelReturn with _$ModelReturn {
  const factory ModelReturn({
    @JsonKey(
      name: r'return',
      required: false,
      includeIfNull: false,
    )
    int? return_,
  }) = _ModelReturn;

  factory ModelReturn.fromJson(Map<String, dynamic> json) =>
      _$ModelReturnFromJson(json);
}

