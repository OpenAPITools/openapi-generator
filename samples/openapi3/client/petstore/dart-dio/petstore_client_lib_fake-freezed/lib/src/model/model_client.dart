//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

// ignore_for_file: unused_element
import 'package:freezed_annotation/freezed_annotation.dart';

part 'model_client.freezed.dart';
part 'model_client.g.dart';


@freezed
abstract class ModelClient with _$ModelClient {
  const factory ModelClient({
    @JsonKey(
      name: r'client',
      required: false,
      includeIfNull: false,
    )
    String? client,
  }) = _ModelClient;

  factory ModelClient.fromJson(Map<String, dynamic> json) =>
      _$ModelClientFromJson(json);
}

