# do not import all endpoints into this module because that uses a lot of memory and stack frames
# if you need the ability to import all endpoints from this module, import them with
# from unit_test_api.paths.request_body_post_nested_anyof_to_check_validation_semantics_request_body import Api

from unit_test_api.path import PathValues

path = PathValues.post_nested_anyof_to_check_validation_semantics_request_body