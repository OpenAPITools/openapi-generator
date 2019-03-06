#!/usr/bin/env python

import os

__author__ = 'Daniel Grossmann-Kavanagh'
__version__ = '0.0.2'

def get_path(rel):
    return os.path.join(
        os.path.abspath(
            os.path.dirname(
                os.path.realpath(__file__)
            )
        ),
        rel 
    )

swagger_ui_2_2_10_path = get_path("vendor/swagger-ui-2.2.10")

swagger_ui_3_17_1_path = get_path("vendor/swagger-ui-3.17.1")

# latest major versions
swagger_ui_2_path = swagger_ui_2_2_10_path
swagger_ui_3_path = swagger_ui_3_17_1_path

# default to swagger 3
swagger_ui_path = swagger_ui_3_path
