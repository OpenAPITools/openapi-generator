import logging
import argparse
import os
import pathlib
import sys


from openapi_spec_validator import validate_spec_url, validate_v2_spec_url
from openapi_spec_validator.exceptions import ValidationError

logger = logging.getLogger(__name__)
logging.basicConfig(
    format='%(asctime)s %(levelname)s %(name)s %(message)s',
    level=logging.WARNING
)


def main(args=None):
    parser = argparse.ArgumentParser()
    parser.add_argument('filename', help="Absolute or relative path to file")
    parser.add_argument(
        '--schema',
        help="OpenAPI schema (default: 3.0.0)",
        type=str,
        choices=['2.0', '3.0.0'],
        default='3.0.0'
    )
    args = parser.parse_args(args)
    filename = args.filename
    filename = os.path.abspath(filename)
    # choose the validator
    if args.schema == '2.0':
        validate_url = validate_v2_spec_url
    elif args.schema == '3.0.0':
        validate_url = validate_spec_url
    # validate
    try:
        validate_url(pathlib.Path(filename).as_uri())
    except ValidationError as e:
        print(e)
        sys.exit(1)
    except Exception as e:
        print(e)
        sys.exit(2)
    else:
        print('OK')


if __name__ == '__main__':
    main()
