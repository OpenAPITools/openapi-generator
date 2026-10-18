//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

// ignore_for_file: unused_element
import 'package:freezed_annotation/freezed_annotation.dart';

part 'model_file.freezed.dart';
part 'model_file.g.dart';


@freezed
abstract class ModelFile with _$ModelFile {
  const factory ModelFile({
    /// Test capitalization
    @JsonKey(
      name: r'sourceURI',
      required: false,
      includeIfNull: false,
    )
    String? sourceURI,
  }) = _ModelFile;

  factory ModelFile.fromJson(Map<String, dynamic> json) =>
      _$ModelFileFromJson(json);
}

