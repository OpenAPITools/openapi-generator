"""
Application data stored by virtualenv.
"""
from __future__ import absolute_import, unicode_literals

import logging
import os
from argparse import Action, ArgumentError

from appdirs import user_data_dir

from .na import AppDataDisabled
from .via_disk_folder import AppDataDiskFolder
from .via_tempdir import TempAppData


class AppDataAction(Action):
    def __call__(self, parser, namespace, values, option_string=None):
        folder = self._check_folder(values)
        if folder is None:
            raise ArgumentError("app data path {} is not valid".format(values))
        setattr(namespace, self.dest, AppDataDiskFolder(folder))

    @staticmethod
    def _check_folder(folder):
        folder = os.path.abspath(folder)
        if not os.path.exists(folder):
            try:
                os.makedirs(folder)
                logging.debug("created app data folder %s", folder)
            except OSError as exception:
                logging.info("could not create app data folder %s due to %r", folder, exception)
                return None
        write_enabled = os.access(folder, os.W_OK)
        if write_enabled:
            return folder
        logging.debug("app data folder %s has no write access", folder)
        return None

    @staticmethod
    def default():
        for folder in AppDataAction._app_data_candidates():
            folder = AppDataAction._check_folder(folder)
            if folder is not None:
                return AppDataDiskFolder(folder)
        return AppDataDisabled()

    @staticmethod
    def _app_data_candidates():
        key = str("VIRTUALENV_OVERRIDE_APP_DATA")
        if key in os.environ:
            yield os.environ[key]
        else:
            yield user_data_dir(appname="virtualenv", appauthor="pypa")


__all__ = (
    "AppDataDiskFolder",
    "TempAppData",
    "AppDataAction",
    "AppDataDisabled",
)
