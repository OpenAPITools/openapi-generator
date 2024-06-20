// Model def

import 'dart:convert';
import 'dart:typed_data';
import 'package:openapi/_internal.dart';
import 'package:xml/xml.dart';

part 'test_inline_freeform_additional_properties_request.reflection.dart';
part 'test_inline_freeform_additional_properties_request.serialization.dart';

//class defination

///
mixin TestInlineFreeformAdditionalPropertiesRequestMixin
    on AdditionalPropertiesMixin<Object?>, $OpenApiObjectMixin {
  UndefinedWrapper<String> get someProperty;
}

///
class TestInlineFreeformAdditionalPropertiesRequest
    with
        $OpenApiObjectMixin,
        AdditionalPropertiesMixin<Object?>,
        TestInlineFreeformAdditionalPropertiesRequestMixin {
  @override
  UndefinedWrapper<String> someProperty;

  @override
  AdditionalProperties<Object?> additionalProperties;

  TestInlineFreeformAdditionalPropertiesRequest.$all({
    required this.someProperty,
    required this.additionalProperties,
  });

  TestInlineFreeformAdditionalPropertiesRequest({
    this.someProperty = const UndefinedWrapper.undefined(),
    this.additionalProperties = const AdditionalProperties(),
  });
}
