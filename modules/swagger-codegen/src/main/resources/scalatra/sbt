#!/usr/bin/env bash
#
# A more capable sbt runner, coincidentally also called sbt.
# Author: Paul Phillips <paulp@typesafe.com>

# todo - make this dynamic
declare -r sbt_release_version="0.13.6"
declare -r sbt_unreleased_version="0.13.6"
declare -r buildProps="project/build.properties"

declare sbt_jar sbt_dir sbt_create sbt_version
declare scala_version sbt_explicit_version
declare verbose noshare batch trace_level log_level
declare sbt_saved_stty debugUs

echoerr () { echo >&2 "$@"; }
vlog ()    { [[ -n "$verbose" ]] && echoerr "$@"; }

# spaces are possible, e.g. sbt.version = 0.13.0
build_props_sbt () {
  [[ -r "$buildProps" ]] && \
    grep '^sbt\.version' "$buildProps" | tr '=' ' ' | awk '{ print $2; }'
}

update_build_props_sbt () {
  local ver="$1"
  local old="$(build_props_sbt)"

  [[ -r "$buildProps" ]] && [[ "$ver" != "$old" ]] && {
    perl -pi -e "s/^sbt\.version\b.*\$/sbt.version=${ver}/" "$buildProps"
    grep -q '^sbt.version[ =]' "$buildProps" || printf "\nsbt.version=%s\n" "$ver" >> "$buildProps"

    vlog "!!!"
    vlog "!!! Updated file $buildProps setting sbt.version to: $ver"
    vlog "!!! Previous value was: $old"
    vlog "!!!"
  }
}

set_sbt_version () {
  sbt_version="${sbt_explicit_version:-$(build_props_sbt)}"
  [[ -n "$sbt_version" ]] || sbt_version=$sbt_release_version
  export sbt_version
}

# restore stty settings (echo in particular)
onSbtRunnerExit() {
  [[ -n "$sbt_saved_stty" ]] || return
  vlog ""
  vlog "restoring stty: $sbt_saved_stty"
  stty "$sbt_saved_stty"
  unset sbt_saved_stty
}

# save stty and trap exit, to ensure echo is reenabled if we are interrupted.
trap onSbtRunnerExit EXIT
sbt_saved_stty="$(stty -g 2>/dev/null)"
vlog "Saved stty: $sbt_saved_stty"

# this seems to cover the bases on OSX, and someone will
# have to tell me about the others.
get_script_path () {
  local path="$1"
  [[ -L "$path" ]] || { echo "$path" ; return; }

  local target="$(readlink "$path")"
  if [[ "${target:0:1}" == "/" ]]; then
    echo "$target"
  else
    echo "${path%/*}/$target"
  fi
}

die() {
  echo "Aborting: $@"
  exit 1
}

make_url () {
  version="$1"

  case "$version" in
        0.7.*) echo "http://simple-build-tool.googlecode.com/files/sbt-launch-0.7.7.jar" ;;
      0.10.* ) echo "$sbt_launch_repo/org.scala-tools.sbt/sbt-launch/$version/sbt-launch.jar" ;;
    0.11.[12]) echo "$sbt_launch_repo/org.scala-tools.sbt/sbt-launch/$version/sbt-launch.jar" ;;
            *) echo "$sbt_launch_repo/org.scala-sbt/sbt-launch/$version/sbt-launch.jar" ;;
  esac
}

init_default_option_file () {
  local overriding_var="${!1}"
  local default_file="$2"
  if [[ ! -r "$default_file" && "$overriding_var" =~ ^@(.*)$ ]]; then
    local envvar_file="${BASH_REMATCH[1]}"
    if [[ -r "$envvar_file" ]]; then
      default_file="$envvar_file"
    fi
  fi
  echo "$default_file"
}

declare -r cms_opts="-XX:+CMSClassUnloadingEnabled -XX:+UseConcMarkSweepGC"
declare -r jit_opts="-XX:ReservedCodeCacheSize=256m -XX:+TieredCompilation"
declare -r default_jvm_opts_common="-Xms512m -Xmx1536m -Xss2m $jit_opts $cms_opts"
declare -r noshare_opts="-Dsbt.global.base=project/.sbtboot -Dsbt.boot.directory=project/.boot -Dsbt.ivy.home=project/.ivy"
declare -r latest_28="2.8.2"
declare -r latest_29="2.9.3"
declare -r latest_210="2.10.4"
declare -r latest_211="2.11.2"

declare -r script_path="$(get_script_path "$BASH_SOURCE")"
declare -r script_name="${script_path##*/}"

# some non-read-onlies set with defaults
declare java_cmd="java"
declare sbt_opts_file="$(init_default_option_file SBT_OPTS .sbtopts)"
declare jvm_opts_file="$(init_default_option_file JVM_OPTS .jvmopts)"
declare sbt_launch_repo="http://typesafe.artifactoryonline.com/typesafe/ivy-releases"

# pull -J and -D options to give to java.
declare -a residual_args
declare -a java_args
declare -a scalac_args
declare -a sbt_commands

# args to jvm/sbt via files or environment variables
declare -a extra_jvm_opts extra_sbt_opts

# if set, use JAVA_HOME over java found in path
[[ -e "$JAVA_HOME/bin/java" ]] && java_cmd="$JAVA_HOME/bin/java"

# directory to store sbt launchers
declare sbt_launch_dir="$HOME/.sbt/launchers"
[[ -d "$sbt_launch_dir" ]] || mkdir -p "$sbt_launch_dir"
[[ -w "$sbt_launch_dir" ]] || sbt_launch_dir="$(mktemp -d -t sbt_extras_launchers.XXXXXX)"

java_version () {
  local version=$("$java_cmd" -version 2>&1 | grep -e 'java version' | awk '{ print $3 }' | tr -d \")
  vlog "Detected Java version: $version"
  echo "${version:2:1}"
}

# MaxPermSize critical on pre-8 jvms but incurs noisy warning on 8+
default_jvm_opts () {
  local v="$(java_version)"
  if [[ $v -ge 8 ]]; then
    echo "$default_jvm_opts_common"
  else
    echo "-XX:MaxPermSize=384m $default_jvm_opts_common"
  fi
}

build_props_scala () {
  if [[ -r "$buildProps" ]]; then
    versionLine="$(grep '^build.scala.versions' "$buildProps")"
    versionString="${versionLine##build.scala.versions=}"
    echo "${versionString%% .*}"
  fi
}

execRunner () {
  # print the arguments one to a line, quoting any containing spaces
  vlog "# Executing command line:" && {
    for arg; do
      if [[ -n "$arg" ]]; then
        if printf "%s\n" "$arg" | grep -q ' '; then
          printf >&2 "\"%s\"\n" "$arg"
        else
          printf >&2 "%s\n" "$arg"
        fi
      fi
    done
    vlog ""
  }

  [[ -n "$batch" ]] && exec </dev/null
  exec "$@"
}

jar_url () {
  make_url "$1"
}

jar_file () {
  echo "$sbt_launch_dir/$1/sbt-launch.jar"
}

download_url () {
  local url="$1"
  local jar="$2"

  echoerr "Downloading sbt launcher for $sbt_version:"
  echoerr "  From  $url"
  echoerr "    To  $jar"

  mkdir -p "${jar%/*}" && {
    if which curl >/dev/null; then
      curl --fail --silent "$url" --output "$jar"
    elif which wget >/dev/null; then
      wget --quiet -O "$jar" "$url"
    fi
  } && [[ -r "$jar" ]]
}

acquire_sbt_jar () {
  sbt_url="$(jar_url "$sbt_version")"
  sbt_jar="$(jar_file "$sbt_version")"

  [[ -r "$sbt_jar" ]] || download_url "$sbt_url" "$sbt_jar"
}

usage () {
  cat <<EOM
Usage: $script_name [options]

Note that options which are passed along to sbt begin with -- whereas
options to this runner use a single dash. Any sbt command can be scheduled
to run first by prefixing the command with --, so --warn, --error and so on
are not special.

Output filtering: if there is a file in the home directory called .sbtignore
and this is not an interactive sbt session, the file is treated as a list of
bash regular expressions. Output lines which match any regex are not echoed.
One can see exactly which lines would have been suppressed by starting this
runner with the -x option.

  -h | -help         print this message
  -v                 verbose operation (this runner is chattier)
  -d, -w, -q         aliases for --debug, --warn, --error (q means quiet)
  -x                 debug this script
  -trace <level>     display stack traces with a max of <level> frames (default: -1, traces suppressed)
  -debug-inc         enable debugging log for the incremental compiler
  -no-colors         disable ANSI color codes
  -sbt-create        start sbt even if current directory contains no sbt project
  -sbt-dir   <path>  path to global settings/plugins directory (default: ~/.sbt/<version>)
  -sbt-boot  <path>  path to shared boot directory (default: ~/.sbt/boot in 0.11+)
  -ivy       <path>  path to local Ivy repository (default: ~/.ivy2)
  -no-share          use all local caches; no sharing
  -offline           put sbt in offline mode
  -jvm-debug <port>  Turn on JVM debugging, open at the given port.
  -batch             Disable interactive mode
  -prompt <expr>     Set the sbt prompt; in expr, 's' is the State and 'e' is Extracted

  # sbt version (default: sbt.version from $buildProps if present, otherwise $sbt_release_version)
  -sbt-force-latest         force the use of the latest release of sbt: $sbt_release_version
  -sbt-version  <version>   use the specified version of sbt (default: $sbt_release_version)
  -sbt-dev                  use the latest pre-release version of sbt: $sbt_unreleased_version
  -sbt-jar      <path>      use the specified jar as the sbt launcher
  -sbt-launch-dir <path>    directory to hold sbt launchers (default: ~/.sbt/launchers)
  -sbt-launch-repo <url>    repo url for downloading sbt launcher jar (default: $sbt_launch_repo)

  # scala version (default: as chosen by sbt)
  -28                       use $latest_28
  -29                       use $latest_29
  -210                      use $latest_210
  -211                      use $latest_211
  -scala-home <path>        use the scala build at the specified directory
  -scala-version <version>  use the specified version of scala
  -binary-version <version> use the specified scala version when searching for dependencies

  # java version (default: java from PATH, currently $(java -version 2>&1 | grep version))
  -java-home <path>         alternate JAVA_HOME

  # passing options to the jvm - note it does NOT use JAVA_OPTS due to pollution
  # The default set is used if JVM_OPTS is unset and no -jvm-opts file is found
  <default>        $(default_jvm_opts)
  JVM_OPTS         environment variable holding either the jvm args directly, or
                   the reference to a file containing jvm args if given path is prepended by '@' (e.g. '@/etc/jvmopts')
                   Note: "@"-file is overridden by local '.jvmopts' or '-jvm-opts' argument.
  -jvm-opts <path> file containing jvm args (if not given, .jvmopts in project root is used if present)
  -Dkey=val        pass -Dkey=val directly to the jvm
  -J-X             pass option -X directly to the jvm (-J is stripped)

  # passing options to sbt, OR to this runner
  SBT_OPTS         environment variable holding either the sbt args directly, or
                   the reference to a file containing sbt args if given path is prepended by '@' (e.g. '@/etc/sbtopts')
                   Note: "@"-file is overridden by local '.sbtopts' or '-sbt-opts' argument.
  -sbt-opts <path> file containing sbt args (if not given, .sbtopts in project root is used if present)
  -S-X             add -X to sbt's scalacOptions (-S is stripped)
EOM
}

addJava () {
  vlog "[addJava] arg = '$1'"
  java_args=( "${java_args[@]}" "$1" )
}
addSbt () {
  vlog "[addSbt] arg = '$1'"
  sbt_commands=( "${sbt_commands[@]}" "$1" )
}
setThisBuild () {
  vlog "[addBuild] args = '$@'"
  local key="$1" && shift
  addSbt "set $key in ThisBuild := $@"
}

addScalac () {
  vlog "[addScalac] arg = '$1'"
  scalac_args=( "${scalac_args[@]}" "$1" )
}
addResidual () {
  vlog "[residual] arg = '$1'"
  residual_args=( "${residual_args[@]}" "$1" )
}
addResolver () {
  addSbt "set resolvers += $1"
}
addDebugger () {
  addJava "-Xdebug"
  addJava "-Xrunjdwp:transport=dt_socket,server=y,suspend=n,address=$1"
}
setScalaVersion () {
  [[ "$1" == *"-SNAPSHOT" ]] && addResolver 'Resolver.sonatypeRepo("snapshots")'
  addSbt "++ $1"
}

process_args ()
{
  require_arg () {
    local type="$1"
    local opt="$2"
    local arg="$3"

    if [[ -z "$arg" ]] || [[ "${arg:0:1}" == "-" ]]; then
      die "$opt requires <$type> argument"
    fi
  }
  while [[ $# -gt 0 ]]; do
    case "$1" in
          -h|-help) usage; exit 1 ;;
                -v) verbose=true && shift ;;
                -d) addSbt "--debug" && shift ;;
                -w) addSbt "--warn" && shift ;;
                -q) addSbt "--error" && shift ;;
                -x) debugUs=true && shift ;;
            -trace) require_arg integer "$1" "$2" && trace_level="$2" && shift 2 ;;
              -ivy) require_arg path "$1" "$2" && addJava "-Dsbt.ivy.home=$2" && shift 2 ;;
        -no-colors) addJava "-Dsbt.log.noformat=true" && shift ;;
         -no-share) noshare=true && shift ;;
         -sbt-boot) require_arg path "$1" "$2" && addJava "-Dsbt.boot.directory=$2" && shift 2 ;;
          -sbt-dir) require_arg path "$1" "$2" && sbt_dir="$2" && shift 2 ;;
        -debug-inc) addJava "-Dxsbt.inc.debug=true" && shift ;;
          -offline) addSbt "set offline := true" && shift ;;
        -jvm-debug) require_arg port "$1" "$2" && addDebugger "$2" && shift 2 ;;
            -batch) batch=true && shift ;;
           -prompt) require_arg "expr" "$1" "$2" && setThisBuild shellPrompt "(s => { val e = Project.extract(s) ; $2 })" && shift 2 ;;

       -sbt-create) sbt_create=true && shift ;;
          -sbt-jar) require_arg path "$1" "$2" && sbt_jar="$2" && shift 2 ;;
      -sbt-version) require_arg version "$1" "$2" && sbt_explicit_version="$2" && shift 2 ;;
 -sbt-force-latest) sbt_explicit_version="$sbt_release_version" && shift ;;
          -sbt-dev) sbt_explicit_version="$sbt_unreleased_version" && shift ;;
   -sbt-launch-dir) require_arg path "$1" "$2" && sbt_launch_dir="$2" && shift 2 ;;
  -sbt-launch-repo) require_arg path "$1" "$2" && sbt_launch_repo="$2" && shift 2 ;;
    -scala-version) require_arg version "$1" "$2" && setScalaVersion "$2" && shift 2 ;;
   -binary-version) require_arg version "$1" "$2" && setThisBuild scalaBinaryVersion "\"$2\"" && shift 2 ;;
       -scala-home) require_arg path "$1" "$2" && setThisBuild scalaHome "Some(file(\"$2\"))" && shift 2 ;;
        -java-home) require_arg path "$1" "$2" && java_cmd="$2/bin/java" && shift 2 ;;
         -sbt-opts) require_arg path "$1" "$2" && sbt_opts_file="$2" && shift 2 ;;
         -jvm-opts) require_arg path "$1" "$2" && jvm_opts_file="$2" && shift 2 ;;

               -D*) addJava "$1" && shift ;;
               -J*) addJava "${1:2}" && shift ;;
               -S*) addScalac "${1:2}" && shift ;;
               -28) setScalaVersion "$latest_28" && shift ;;
               -29) setScalaVersion "$latest_29" && shift ;;
              -210) setScalaVersion "$latest_210" && shift ;;
              -211) setScalaVersion "$latest_211" && shift ;;

                 *) addResidual "$1" && shift ;;
    esac
  done
}

# process the direct command line arguments
process_args "$@"

# skip #-styled comments and blank lines
readConfigFile() {
  while read line; do
    [[ $line =~ ^# ]] || [[ -z $line ]] || echo "$line"
  done < "$1"
}

# if there are file/environment sbt_opts, process again so we
# can supply args to this runner
if [[ -r "$sbt_opts_file" ]]; then
  vlog "Using sbt options defined in file $sbt_opts_file"
  while read opt; do extra_sbt_opts+=("$opt"); done < <(readConfigFile "$sbt_opts_file")
elif [[ -n "$SBT_OPTS" && ! ("$SBT_OPTS" =~ ^@.*) ]]; then
  vlog "Using sbt options defined in variable \$SBT_OPTS"
  extra_sbt_opts=( $SBT_OPTS )
else
  vlog "No extra sbt options have been defined"
fi

[[ -n "${extra_sbt_opts[*]}" ]] && process_args "${extra_sbt_opts[@]}"

# reset "$@" to the residual args
set -- "${residual_args[@]}"
argumentCount=$#

# set sbt version
set_sbt_version

# only exists in 0.12+
setTraceLevel() {
  case "$sbt_version" in
    "0.7."* | "0.10."* | "0.11."* ) echoerr "Cannot set trace level in sbt version $sbt_version" ;;
                                 *) setThisBuild traceLevel $trace_level ;;
  esac
}

# set scalacOptions if we were given any -S opts
[[ ${#scalac_args[@]} -eq 0 ]] || addSbt "set scalacOptions in ThisBuild += \"${scalac_args[@]}\""

# Update build.properties on disk to set explicit version - sbt gives us no choice
[[ -n "$sbt_explicit_version" ]] && update_build_props_sbt "$sbt_explicit_version"
vlog "Detected sbt version $sbt_version"

[[ -n "$scala_version" ]] && vlog "Overriding scala version to $scala_version"

# no args - alert them there's stuff in here
(( argumentCount > 0 )) || {
  vlog "Starting $script_name: invoke with -help for other options"
  residual_args=( shell )
}

# verify this is an sbt dir or -create was given
[[ -r ./build.sbt || -d ./project || -n "$sbt_create" ]] || {
  cat <<EOM
$(pwd) doesn't appear to be an sbt project.
If you want to start sbt anyway, run:
  $0 -sbt-create

EOM
  exit 1
}

# pick up completion if present; todo
[[ -r .sbt_completion.sh ]] && source .sbt_completion.sh

# no jar? download it.
[[ -r "$sbt_jar" ]] || acquire_sbt_jar || {
  # still no jar? uh-oh.
  echo "Download failed. Obtain the jar manually and place it at $sbt_jar"
  exit 1
}

if [[ -n "$noshare" ]]; then
  for opt in ${noshare_opts}; do
    addJava "$opt"
  done
else
  case "$sbt_version" in
    "0.7."* | "0.10."* | "0.11."* | "0.12."* )
      [[ -n "$sbt_dir" ]] || {
        sbt_dir="$HOME/.sbt/$sbt_version"
        vlog "Using $sbt_dir as sbt dir, -sbt-dir to override."
      }
    ;;
  esac

  if [[ -n "$sbt_dir" ]]; then
    addJava "-Dsbt.global.base=$sbt_dir"
  fi
fi

if [[ -r "$jvm_opts_file" ]]; then
  vlog "Using jvm options defined in file $jvm_opts_file"
  while read opt; do extra_jvm_opts+=("$opt"); done < <(readConfigFile "$jvm_opts_file")
elif [[ -n "$JVM_OPTS" && ! ("$JVM_OPTS" =~ ^@.*) ]]; then
  vlog "Using jvm options defined in \$JVM_OPTS variable"
  extra_jvm_opts=( $JVM_OPTS )
else
  vlog "Using default jvm options"
  extra_jvm_opts=( $(default_jvm_opts) )
fi

# traceLevel is 0.12+
[[ -n "$trace_level" ]] && setTraceLevel

main () {
  execRunner "$java_cmd" \
    "${extra_jvm_opts[@]}" \
    "${java_args[@]}" \
    -jar "$sbt_jar" \
    "${sbt_commands[@]}" \
    "${residual_args[@]}"
}

# sbt inserts this string on certain lines when formatting is enabled:
#   val OverwriteLine = "\r\u001BM\u001B[2K"
# ...in order not to spam the console with a million "Resolving" lines.
# Unfortunately that makes it that much harder to work with when
# we're not going to print those lines anyway. We strip that bit of
# line noise, but leave the other codes to preserve color.
mainFiltered () {
  local ansiOverwrite='\r\x1BM\x1B[2K'
  local excludeRegex=$(egrep -v '^#|^$' ~/.sbtignore | paste -sd'|' -)

  echoLine () {
    local line="$1"
    local line1="$(echo "$line" | sed -r 's/\r\x1BM\x1B\[2K//g')"       # This strips the OverwriteLine code.
    local line2="$(echo "$line1" | sed -r 's/\x1B\[[0-9;]*[JKmsu]//g')" # This strips all codes - we test regexes against this.

    if [[ $line2 =~ $excludeRegex ]]; then
      [[ -n $debugUs ]] && echo "[X] $line1"
    else
      [[ -n $debugUs ]] && echo "    $line1" || echo "$line1"
    fi
  }

  echoLine "Starting sbt with output filtering enabled."
  main | while read -r line; do echoLine "$line"; done
}

# Only filter if there's a filter file and we don't see a known interactive command.
# Obviously this is super ad hoc but I don't know how to improve on it. Testing whether
# stdin is a terminal is useless because most of my use cases for this filtering are
# exactly when I'm at a terminal, running sbt non-interactively.
shouldFilter () { [[ -f ~/.sbtignore ]] && ! egrep -q '\b(shell|console|consoleProject)\b' <<<"${residual_args[@]}"; }

# run sbt
if shouldFilter; then mainFiltered; else main; fi
