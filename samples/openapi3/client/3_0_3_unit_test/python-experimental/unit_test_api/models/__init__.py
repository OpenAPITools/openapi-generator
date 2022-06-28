# coding: utf-8

# flake8: noqa

# import all models into this package
# if you have many models here with many references from one model to another this may
# raise a RecursionError
# to avoid this, import only the models that you directly need like:
# from from unit_test_api.model.pet import Pet
# or import this package, but before doing it, use:
# import sys
# sys.setrecursionlimit(n)

from unit_test_api.model.nested_oneof_to_check_validation_semantics import NestedOneofToCheckValidationSemantics
from unit_test_api.model.oneof import Oneof
from unit_test_api.model.oneof_complex_types import OneofComplexTypes
from unit_test_api.model.oneof_with_base_schema import OneofWithBaseSchema
from unit_test_api.model.oneof_with_boolean_schemas_all_false import OneofWithBooleanSchemasAllFalse
from unit_test_api.model.oneof_with_boolean_schemas_all_true import OneofWithBooleanSchemasAllTrue
from unit_test_api.model.oneof_with_boolean_schemas_more_than_one_true import OneofWithBooleanSchemasMoreThanOneTrue
from unit_test_api.model.oneof_with_boolean_schemas_one_true import OneofWithBooleanSchemasOneTrue
from unit_test_api.model.oneof_with_empty_schema import OneofWithEmptySchema
from unit_test_api.model.oneof_with_required import OneofWithRequired
