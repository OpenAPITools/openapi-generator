from typing import Optional


class CompilerOptions:
    def __init__(self, strip_asserts: bool = False, multi_file: bool = False,
                 verbose: bool = False, separate: bool = False,
                 target_dir: Optional[str] = None,
                 include_runtime_files: Optional[bool] = None) -> None:
        self.strip_asserts = strip_asserts
        self.multi_file = multi_file
        self.verbose = verbose
        self.separate = separate
        self.global_opts = not separate
        self.target_dir = target_dir or 'build'
        self.include_runtime_files = (
            include_runtime_files if include_runtime_files is not None else not multi_file
        )
