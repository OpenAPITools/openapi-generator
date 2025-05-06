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

     this.someproperty,
  });

  @JsonKey(
    
    name: r'someProperty',
    required: false,
    includeIfNull: false,
  )


  final String? someproperty;





    @override
    bool operator ==(Object other) => identical(this, other) || other is TestInlineFreeformAdditionalPropertiesRequest &&
      other.someproperty == someproperty;

    @override
    int get hashCode =>
        someproperty.hashCode;

  factory TestInlineFreeformAdditionalPropertiesRequest.fromJson(Map<String, dynamic> json) => _$TestInlineFreeformAdditionalPropertiesRequestFromJson(json);

  Map<String, dynamic> toJson() => _$TestInlineFreeformAdditionalPropertiesRequestToJson(this);

  @override
  String toString() {
    return toJson().toString();
  }

}

