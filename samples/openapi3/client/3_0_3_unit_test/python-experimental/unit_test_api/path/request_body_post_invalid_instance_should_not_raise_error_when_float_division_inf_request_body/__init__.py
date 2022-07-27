# do not import all endpoints into this module because that uses a lot of memory and stack frames
# if you need the ability to import all endpoints from this module, import them with
# from unit_test_api.paths.request_body_post_invalid_instance_should_not_raise_error_when_float_division_inf_request_body import Api

from unit_test_api.path import PathValues

path = PathValues.post_invalid_instance_should_not_raise_error_when_float_division_inf_request_body