<p align="center">
  <img src="https://raw.githubusercontent.com/veelenga/bin/master/ameba/logo.png" width="800">
  <h3 align="center">Ameba</h3>
  <p align="center">Code style linter for Crystal<p>
  <p align="center">
    <sup>
      <i> (a single-celled animal that catches food and moves about by extending fingerlike projections of protoplasm) </i>
    </sup>
  </p>
  <p align="center">
    <a href="https://travis-ci.org/crystal-ameba/ameba"><img src="https://travis-ci.org/crystal-ameba/ameba.svg?branch=master"></a>
    <a href="https://github.com/crystal-ameba/ameba/releases"><img src="https://img.shields.io/github/release/crystal-ameba/ameba.svg?maxAge=360"></a>
    <a href="https://github.com/crystal-ameba/ameba/blob/master/LICENSE"><img src="https://img.shields.io/github/license/crystal-ameba/ameba.svg"></a>
  <a href="https://gitter.im/veelenga/ameba?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge"><img src="https://badges.gitter.im/veelenga/ameba.svg"></a>
  </p>
</p>

- [About](#about)
- [Usage](#usage)
  * [Run in parallel](#run-in-parallel)
- [Installation](#installation)
  * [As a project dependency:](#as-a-project-dependency)
  * [OS X](#os-x)
  * [Docker](#docker)
  * [From sources](#from-sources)
- [Configuration](#configuration)
  * [Sources](#sources)
  * [Rules](#rules)
  * [Explain issues](#explain-issues)
  * [Inline disabling](#inline-disabling)
- [Editors & integrations](#editors--integrations)
- [Credits & inspirations](#credits--inspirations)
- [Contributors](#contributors)

## About

Ameba is a static code analysis tool for the Crystal language.
It enforces a consistent [Crystal code style](https://crystal-lang.org/docs/conventions/coding_style.html),
also catches code smells and wrong code constructions.

See also [Roadmap](https://github.com/crystal-ameba/ameba/wiki).

## Usage

Run `ameba` binary within your project directory to catch code issues:

```sh
$ ameba
Inspecting 107 files.

...............F.....................F....................................................................

src/ameba/formatter/flycheck_formatter.cr:4:33
[W] Lint/UnusedArgument: Unused argument `location`
> source.issues.each do |e, location|
                            ^

src/ameba/formatter/base_formatter.cr:12:7
[W] Lint/UselessAssign: Useless assignment to variable `s`
> return s += issues.size
         ^

Finished in 542.64 milliseconds

129 inspected, 2 failures.

```

### Run in parallel

Starting from 0.31.0 Crystal [supports parallelism](https://crystal-lang.org/2019/09/06/parallelism-in-crystal.html).
It allows to run linting in parallel too.
In order to take advantage of this feature you need to build ameba with preview_mt support:

```sh
$ crystal build src/cli.cr -Dpreview_mt -o bin/ameba
$ make install
```

Some quick benchmark results measured while running Ameba on Crystal repo:

```sh
$ CRYSTAL_WORKERS=1 ameba #=> 29.11 seconds
$ CRYSTAL_WORKERS=2 ameba #=> 19.49 seconds
$ CRYSTAL_WORKERS=4 ameba #=> 13.48 seconds
$ CRYSTAL_WORKERS=8 ameba #=> 10.14 seconds
```

## Installation

### As a project dependency:

Add this to your application's `shard.yml`:

```yaml
development_dependencies:
  ameba:
    github: crystal-ameba/ameba
    version: ~> 0.13.0
```

Build `bin/ameba` binary within your project directory while running `shards install`.

You may also want to use it on [Travis](travis-ci.org):

```yaml
# .travis.yml
language: crystal
install:
  - shards install
script:
  - crystal spec
  - crystal bin/ameba.cr
```

Using this config Ameba will inspect files just after the specs run. Travis will also fail
the build if some problems detected.

### OS X

```sh
$ brew tap veelenga/tap
$ brew install ameba
```

### Docker

Build the image:

```sh
$ docker build -t crystal-ameba/ameba .
```

To use the resulting image on a local source folder, mount the current (or target) directory into `/src`:

```sh
$ docker run -v $(pwd):/src crystal-ameba/ameba
```

Also available on DockerHub: https://hub.docker.com/r/veelenga/ameba

### From sources

```sh
$ git clone https://github.com/crystal-ameba/ameba && cd ameba
$ make install
```

## Configuration

Default configuration file is `.ameba.yml`.
It allows to configure rule properties, disable specific rules and exclude sources from the rules.

Generate new file by running `ameba --gen-config`.

### Sources

**List of sources to run Ameba on can be configured globally via:**

- `Globs` section - an array of wildcards (or paths) to include to the
  inspection. Defaults to `%w(**/*.cr !lib)`, meaning it includes all project
  files with `*.cr` extension except those which exist in `lib` folder.
- `Excluded` section - an array of wildcards (or paths) to exclude from the
  source list defined by `Globs`. Defaults to an empty array.

In this example we define default globs and exclude `src/compiler` folder:

``` yaml
Globs:
  - **/*.cr
  - !lib
  
Excluded:
  - src/compiler
```

**Specific sources can be excluded at rule level**:

``` yaml
Style/RedundantBegin:
  Excluded:
  - src/server/processor.cr
  - src/server/api.cr
```

### Rules

One or more rules, or a one or more group of rules can be included or excluded
via command line arguments:

```sh
$ ameba --only   Lint/Syntax # runs only Lint/Syntax rule
$ ameba --only   Style,Lint  # runs only rules from Style and Lint groups
$ ameba --except Lint/Syntax # runs all rules except Lint/Syntax
$ ameba --except Style,Lint  # runs all rules except rules in Style and Lint groups
```

Or through the configuration file:

``` yaml
Style/RedundantBegin:
  Enabled: false
```

### Explain issues

Ameba allows you to dig deeper into an issue, by showing you details about the issue
and the reasoning by it being reported.

To be convenient, you can just copy-paste the `PATH:line:column` string from the
report and paste behind the `ameba` command to check it out.

```sh
$ ameba crystal/command/format.cr:26:83           # show explanation for the issue
$ ameba --explain crystal/command/format.cr:26:83 # same thing
```

### Inline disabling

One or more rules or one or more group of rules can be disabled using inline directives:

```crystal
# ameba:disable Style/LargeNumbers
time = Time.epoch(1483859302)

time = Time.epoch(1483859302) # ameba:disable Style/LargeNumbers, Lint/UselessAssign

time = Time.epoch(1483859302) # ameba:disable Style, Lint
```

## Editors & integrations

 * Vim: [vim-crystal](https://github.com/rhysd/vim-crystal), [Ale](https://github.com/w0rp/ale)
 * Emacs: [ameba.el](https://github.com/crystal-ameba/ameba.el)
 * Sublime Text: [Sublime Linter Ameba](https://github.com/epergo/SublimeLinter-contrib-ameba)
 * VSCode: [vscode-crystal-ameba](https://github.com/crystal-ameba/vscode-crystal-ameba)
 * Codacy: [codacy-ameba](https://github.com/codacy/codacy-ameba)
 * GitHub Actions: [github-action](https://github.com/crystal-ameba/github-action)

## Credits & inspirations

- [Crystal Language](https://crystal-lang.org)
- [Rubocop](https://rubocop.readthedocs.io/en/latest/)
- [Credo](http://credo-ci.org/)
- [Dogma](https://github.com/lpil/dogma)

## Contributors

- [veelenga](https://github.com/veelenga) Vitalii Elenhaupt - creator, maintainer
