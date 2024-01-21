//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

// ignore_for_file: unused_element
import 'package:json_annotation/json_annotation.dart';

part 'test_inline_freeform_additional_properties_request.g.dart';


@JsonSerializable(
  checked: true,
  createToJson: true,
  disallowUnrecognizedKeys: false,
  explicitToJson: true,
)
class TestInlineFreeformAdditionalPropertiesRequest {
  /// Returns a new [TestInlineFreeformAdditionalPropertiesRequest] instance.
  TestInlineFreeformAdditionalPropertiesRequest({

     this.someProperty,
  });

  @JsonKey(
    
    name: r'someProperty',
    required: false,
    includeIfNull: false
  )


  final String? someProperty;



  @override
  bool operator ==(Object other) => identical(this, other) || other is TestInlineFreeformAdditionalPropertiesRequest &&
     other.someProperty == someProperty;

  @override
  int get hashCode =>
    someProperty.hashCode;

  factory TestInlineFreeformAdditionalPropertiesRequest.fromJson(Map<String, dynamic> json) => _$TestInlineFreeformAdditionalPropertiesRequestFromJson(json);

  Map<String, dynamic> toJson() => _$TestInlineFreeformAdditionalPropertiesRequestToJson(this);

  @override
  String toString() {
    return toJson().toString();
  }

}

