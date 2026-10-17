//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

// ignore_for_file: unused_element
import 'package:freezed_annotation/freezed_annotation.dart';

part 'test_inline_freeform_additional_properties_request.freezed.dart';
part 'test_inline_freeform_additional_properties_request.g.dart';


@freezed
abstract class TestInlineFreeformAdditionalPropertiesRequest with _$TestInlineFreeformAdditionalPropertiesRequest {
  const factory TestInlineFreeformAdditionalPropertiesRequest({
    @JsonKey(
      name: r'someProperty',
      required: false,
      includeIfNull: false,
    )
    String? someProperty,
  }) = _TestInlineFreeformAdditionalPropertiesRequest;

  factory TestInlineFreeformAdditionalPropertiesRequest.fromJson(Map<String, dynamic> json) =>
      _$TestInlineFreeformAdditionalPropertiesRequestFromJson(json);
}

