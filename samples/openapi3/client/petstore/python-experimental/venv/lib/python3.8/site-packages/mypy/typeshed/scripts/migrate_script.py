"""
Ad-hoc script to migrate typeshed to a new directory structure proposed in
https://github.com/python/typeshed/issues/2491#issuecomment-611607557
"""

import ast
import os
import os.path
import shutil
from dataclasses import dataclass
from typing import List, Optional, Set, Tuple

# These names may be still discussed so I make them constants.
STDLIB_NAMESPACE = "stdlib"
THIRD_PARTY_NAMESPACE = "stubs"
DEFAULT_VERSION = "0.1"
DEFAULT_PY3_VERSION = "3.6"
PY2_NAMESPACE = "python2"
OUTPUT_DIR = "out"

# Third party imports (type ignored) of missing stubs.
MISSING_WHITELIST = {
    "thrift",
}

# Manually collected special cases where distribution name and
# package name are different.
package_to_distribution = {
    "_pytest": "pytest",
    "yaml": "PyYAML",
    "typing_extensions": "typing-extensions",
    "mypy_extensions": "mypy-extensions",
    "pyre_extensions": "pyre-extensions",
    "attr": "attrs",
    "concurrent": "futures",
    "Crypto": "pycrypto",
    "datetimerange": "DateTimeRange",
    "dateutil": "python-dateutil",
    "enum": "enum34",
    "flask": "Flask",
    "gflags": "python-gflags",
    "google": "protobuf",
    "jinja2": "Jinja2",
    "markupsafe": "MarkupSafe",
    "OpenSSL": "openssl-python",
    "pymysql": "PyMySQL",
    "pyVmomi": "pyvmomi",
    "routes": "Routes",
    "typed_ast": "typed-ast",
    "werkzeug": "Werkzeug",
}

known_versions = {
    "mypy-extensions": "0.4",
    "typing-extensions": "3.7",
    "typed-ast": "1.4",
}


# Classes with "Package" in name represent both packages and modules.
# The latter two are distinguished by is_dir flag.
class PackageBase:
    """Common attributes for packages/modules"""

    path: str  # full initial path like stdlib/2and3/argparse.pyi
    is_dir: bool

    @property
    def name(self) -> str:
        _, tail = os.path.split(self.path)
        if self.is_dir:
            assert not tail.endswith(".pyi")
            return tail
        assert tail.endswith(".pyi")
        name, _ = os.path.splitext(tail)
        return name


@dataclass
class StdLibPackage(PackageBase):
    """Package/module in standard library."""

    path: str
    py_version: Optional[str]  # Can be omitted for Python 2 only packages.
    is_dir: bool


@dataclass
class ThirdPartyPackage(PackageBase):
    path: str
    py2_compatible: bool
    py3_compatible: bool
    is_dir: bool
    requires: List[str]  # distributions this depends on


def add_stdlib_packages_from(subdir: str, packages: List[StdLibPackage], py_version: Optional[str]) -> None:
    """Add standard library packages/modules from a given stdlib/xxx subdirectory.

    Append to packages list in-place, use py_version as the minimal supported version.
    """
    for name in os.listdir(subdir):
        path = os.path.join(subdir, name)
        packages.append(StdLibPackage(path, py_version, is_dir=os.path.isdir(path)))


def collect_stdlib_packages() -> Tuple[List[StdLibPackage], List[StdLibPackage]]:
    """Collect standard library packages/modules from all current stdlib/xxx sub-directories."""
    stdlib: List[StdLibPackage] = []
    py2_stdlib: List[StdLibPackage] = []
    # These will go to a separate subdirectory.
    add_stdlib_packages_from("stdlib/2", py2_stdlib, None)
    add_stdlib_packages_from("stdlib/2and3", stdlib, "2.7")
    # Use oldest currently supported version for Python 3 packages/modules.
    add_stdlib_packages_from("stdlib/3", stdlib, DEFAULT_PY3_VERSION)
    for version in ("3.7", "3.8", "3.9"):
        subdir = os.path.join("stdlib", version)
        if os.path.isdir(subdir):
            add_stdlib_packages_from(subdir, stdlib, version)
    return stdlib, py2_stdlib


def add_third_party_packages_from(
    subdir: str, packages: List[ThirdPartyPackage], py2_compatible: bool, py3_compatible: bool
) -> None:
    """Add third party packages/modules from a given third_party/xxx subdirectory."""
    for name in os.listdir(subdir):
        path = os.path.join(subdir, name)
        packages.append(ThirdPartyPackage(path, py2_compatible, py3_compatible, requires=[], is_dir=os.path.isdir(path)))


def collect_third_party_packages() -> Tuple[List[ThirdPartyPackage], List[ThirdPartyPackage]]:
    """Collect third party packages/modules from all current third_party/xxx sub-directories."""
    third_party: List[ThirdPartyPackage] = []
    py2_third_party: List[ThirdPartyPackage] = []
    add_third_party_packages_from("third_party/3", third_party, py2_compatible=False, py3_compatible=True)
    add_third_party_packages_from("third_party/2and3", third_party, py2_compatible=True, py3_compatible=True)
    # We special-case Python 2 for third party packages like six.
    subdir = "third_party/2"
    py3_packages = os.listdir("third_party/3")
    for name in os.listdir(subdir):
        path = os.path.join(subdir, name)
        package = ThirdPartyPackage(path, py2_compatible=True, py3_compatible=False, requires=[], is_dir=os.path.isdir(path))
        if name in py3_packages:
            # If there is a package with the same name in /2 and /3, we add the former to
            # a separate list, packages from there will be put into /python2 sub-directories.
            py2_third_party.append(package)
        else:
            third_party.append(package)
    return third_party, py2_third_party


def get_top_imported_names(file: str) -> Set[str]:
    """Collect names imported in given file.

    We only collect top-level names, i.e. `from foo.bar import baz`
    will only add `foo` to the list.
    """
    if not file.endswith(".pyi"):
        return set()
    with open(os.path.join(file), "rb") as f:
        content = f.read()
    parsed = ast.parse(content)
    top_imported = set()
    for node in ast.walk(parsed):
        if isinstance(node, ast.Import):
            for name in node.names:
                top_imported.add(name.name.split(".")[0])
        elif isinstance(node, ast.ImportFrom):
            if node.level > 0:
                # Relative imports always refer to the current package.
                continue
            assert node.module
            top_imported.add(node.module.split(".")[0])
    return top_imported


def populate_requirements(
    package: ThirdPartyPackage, stdlib: List[str], py2_stdlib: List[str], known_distributions: Set[str]
) -> None:
    """Generate requirements using imports found in a package."""
    assert not package.requires, "Populate must be called once"
    if not package.is_dir:
        all_top_imports = get_top_imported_names(package.path)
    else:
        all_top_imports = set()
        for dir_path, _, file_names in os.walk(package.path):
            for file_name in file_names:
                all_top_imports |= get_top_imported_names(os.path.join(dir_path, file_name))

    # Generate dependencies using collected imports.
    requirements = set()
    for name in all_top_imports:
        # Note: dependencies are between distributions, not packages.
        distribution = package_to_distribution.get(name, name)
        if package.py3_compatible and name not in stdlib:
            if distribution in known_distributions:
                requirements.add(distribution)
            else:
                # Likely a conditional import.
                assert distribution in py2_stdlib or distribution in MISSING_WHITELIST
        if package.py2_compatible and name not in py2_stdlib:
            if distribution in known_distributions:
                requirements.add(distribution)
            else:
                # Likely a conditional import.
                assert distribution in stdlib or distribution in MISSING_WHITELIST
    # Remove dependency to itself generated by absolute imports.
    current_distribution = package_to_distribution.get(package.name, package.name)
    package.requires = sorted(requirements - {current_distribution})


def generate_versions(packages: List[StdLibPackage]) -> str:
    """Generate the stdlib/VERSIONS file for packages/modules."""
    lines = []
    for package in packages:
        assert package.py_version is not None
        lines.append(f"{package.name}: {package.py_version}")
    return "\n".join(sorted(lines))


def copy_stdlib(packages: List[StdLibPackage], py2_packages: List[StdLibPackage]) -> None:
    """Refactor the standard library part using collected metadata."""
    stdlib_dir = os.path.join(OUTPUT_DIR, STDLIB_NAMESPACE)
    os.makedirs(stdlib_dir, exist_ok=True)

    # Write version metadata.
    with open(os.path.join(stdlib_dir, "VERSIONS"), "w") as f:
        f.write(generate_versions(packages))
        f.write("\n")

    # Copy stdlib/2and3 and stdlib/3 packages/modules.
    for package in packages:
        if not package.is_dir:
            shutil.copy(package.path, stdlib_dir)
        else:
            shutil.copytree(package.path, os.path.join(stdlib_dir, package.name))

    # Copy stdlib/2 packages/modules to a nested /python namespace.
    if py2_packages:
        py2_stdlib_dir = os.path.join(stdlib_dir, PY2_NAMESPACE)
        os.makedirs(py2_stdlib_dir, exist_ok=True)
        for package in py2_packages:
            if not package.is_dir:
                shutil.copy(package.path, py2_stdlib_dir)
            else:
                shutil.copytree(package.path, os.path.join(py2_stdlib_dir, package.name))


def generate_metadata(package: ThirdPartyPackage, py2_packages: List[str]) -> str:
    """Generate METADATA.toml for a given package.

    Only add compatibility flags if they are different from default values:
    python2 = false, python3 = true.

    Note: the metadata should be generated per distribution, but we just use
    an arbitrary package to populate it, since it should be the same for all
    packages.
    """
    version = known_versions.get(
        package_to_distribution.get(package.name, package.name),
        DEFAULT_VERSION,
    )
    lines = [f'version = "{version}"']
    if package.py2_compatible or package.name in py2_packages:
        # Note: for packages like six that appear in both normal and Python 2 only
        # lists we force set python2 = true.
        lines.append("python2 = true")
    if not package.py3_compatible:
        lines.append("python3 = false")
    if package.requires:
        distributions = [f'"types-{package_to_distribution.get(dep, dep)}"' for dep in package.requires]
        lines.append(f"requires = [{', '.join(distributions)}]")
    return "\n".join(lines)


def copy_third_party(packages: List[ThirdPartyPackage], py2_packages: List[ThirdPartyPackage]) -> None:
    """Refactor the third party part using collected metadata."""
    third_party_dir = os.path.join(OUTPUT_DIR, THIRD_PARTY_NAMESPACE)
    os.makedirs(third_party_dir, exist_ok=True)

    # Note: these include Python 3 versions of packages like six.
    for package in packages:
        distribution = package_to_distribution.get(package.name, package.name)
        distribution_dir = os.path.join(third_party_dir, distribution)
        os.makedirs(distribution_dir, exist_ok=True)
        metadata_file = os.path.join(distribution_dir, "METADATA.toml")
        if not os.path.isfile(metadata_file):
            # Write metadata once.
            # TODO: check consistency between different packages in same distribution?
            with open(metadata_file, "w") as f:
                f.write(generate_metadata(package, [package.name for package in py2_packages]))
                f.write("\n")
        if not package.is_dir:
            shutil.copy(package.path, distribution_dir)
        else:
            shutil.copytree(package.path, os.path.join(distribution_dir, package.name))

    # Add Python 2 counterparts of packages like six (with different stubs) to nested
    # namespaces like six/python2/six.
    for package in py2_packages:
        distribution = package_to_distribution.get(package.name, package.name)
        distribution_dir = os.path.join(third_party_dir, distribution, PY2_NAMESPACE)
        os.makedirs(distribution_dir, exist_ok=True)
        if not package.is_dir:
            shutil.copy(package.path, distribution_dir)
        else:
            shutil.copytree(package.path, os.path.join(distribution_dir, package.name))


def main() -> None:
    # Collect metadata for Python 2 and 3, and Python 2 only standard library
    # packages/modules. The latter will go to a separate nested namespace.
    stdlib, py2_stdlib = collect_stdlib_packages()
    third_party, py2_third_party = collect_third_party_packages()

    # Collect standard library names to filter out from dependencies.
    stdlib_names = [package.name for package in stdlib]
    py2_stdlib_names = [package.name for package in py2_stdlib]
    py2_stdlib_names += [package.name for package in stdlib if package.py_version == "2.7"]

    # Collect all known distributions (for sanity checks).
    known_distributions = {package_to_distribution.get(package.name, package.name) for package in third_party + py2_third_party}

    # Compute dependencies between third party packages/modules to populate metadata.
    for package in third_party + py2_third_party:
        populate_requirements(package, stdlib_names, py2_stdlib_names, known_distributions)

    # Copy the files to a separate location (to not clobber the root directory).
    if not os.path.isdir(OUTPUT_DIR):
        os.mkdir(OUTPUT_DIR)
    copy_stdlib(stdlib, py2_stdlib)
    copy_third_party(third_party, py2_third_party)


if __name__ == "__main__":
    main()
