# -*- coding: utf-8 -*-

import datetime
import json
import numbers
import sys
import time

import click
import yaml

try:
    from urllib.parse import urlsplit, urlunsplit
except ImportError:  # NOQA
    from urlparse import urlsplit, urlunsplit


# global state is evil!
# anyway, we are using this as a convenient hack to switch output formats
GLOBAL_STATE = {'output_format': 'text'}


def is_json_output():
    return GLOBAL_STATE.get('output_format') == 'json'


def is_yaml_output():
    return GLOBAL_STATE.get('output_format') == 'yaml'


def is_tsv_output():
    return GLOBAL_STATE.get('output_format') == 'tsv'


def is_text_output():
    return GLOBAL_STATE.get('output_format') == 'text'


def secho(*args, **kwargs):
    args = list(args)
    if len(args) > 0:
        args[0] = '{}'.format(args[0])
    if 'err' not in kwargs:
        if not sys.stdout.isatty():
            kwargs['err'] = True
        if not is_text_output():
            kwargs['err'] = True

    click.secho(*args, **kwargs)


def action(msg, **kwargs):
    secho(msg.format(**kwargs), nl=False, bold=True)


def ok(msg=' OK', **kwargs):
    secho(msg, fg='green', bold=True, **kwargs)


def error(msg, **kwargs):
    secho(msg, fg='red', bold=True, **kwargs)


def fatal_error(msg, **kwargs):
    error(msg, **kwargs)
    sys.exit(1)


def warning(msg, **kwargs):
    secho(msg, fg='yellow', bold=True, **kwargs)


def info(msg):
    secho(msg, fg='blue', bold=True)


class OutputFormat:

    def __init__(self, fmt):
        self.fmt = fmt
        self._old_fmt = None

    def __enter__(self):
        self._old_fmt = GLOBAL_STATE.get('output_format')
        GLOBAL_STATE['output_format'] = self.fmt

    def __exit__(self, exc_type, exc_val, exc_tb):
        GLOBAL_STATE['output_format'] = self._old_fmt


class Action:

    def __init__(self, msg, ok_msg=' OK', nl=False, **kwargs):
        self.msg = msg
        self.ok_msg = ok_msg
        self.msg_args = kwargs
        self.nl = nl
        self.errors = []
        self._suppress_exception = False

    def __enter__(self):
        action(self.msg, **self.msg_args)
        if self.nl:
            secho('')
        return self

    def __exit__(self, exc_type, exc_val, exc_tb):
        if exc_type is None:
            if not self.errors:
                ok(self.ok_msg)
        elif not self._suppress_exception:
            error(' EXCEPTION OCCURRED: {}'.format(exc_val))

    def fatal_error(self, msg, **kwargs):
        self._suppress_exception = True  # Avoid printing "EXCEPTION OCCURRED: -1" on exit
        fatal_error(' {}'.format(msg), **kwargs)

    def error(self, msg, **kwargs):
        error(' {}'.format(msg), **kwargs)
        self.errors.append(msg)

    def progress(self):
        secho(' .', nl=False)

    def warning(self, msg, **kwargs):
        warning(' {}'.format(msg), **kwargs)
        self.errors.append(msg)

    def ok(self, msg):
        self.ok_msg = ' {}'.format(msg)


def get_now():
    return datetime.datetime.now()


def format_time(ts):
    if ts == 0:
        return ''
    now = get_now()
    try:
        dt = datetime.datetime.fromtimestamp(ts)
    except:
        return ts
    diff = now - dt
    s = diff.total_seconds()
    if s > (3600 * 49):
        t = '{:.0f}d'.format(s / (3600 * 24))
    elif s > 3600:
        t = '{:.0f}h'.format(s / 3600)
    elif s > 70:
        t = '{:.0f}m'.format(s / 60)
    else:
        t = '{:.0f}s'.format(s)
    return '{} ago'.format(t)


def format(col, val):
    if val is None:
        val = ''
    elif col.endswith('_time'):
        val = format_time(val)
    elif isinstance(val, bool):
        val = 'yes' if val else 'no'
    else:
        val = str(val)
    return val


def print_tsv_table(cols, rows):
    sys.stdout.write('\t'.join(cols))
    sys.stdout.write('\n')
    for row in rows:
        first_col = True
        for col in cols:
            if not first_col:
                sys.stdout.write('\t')
            val = row.get(col)
            sys.stdout.write(format(col, val))
            first_col = False
        sys.stdout.write('\n')


def print_table(cols, rows, styles=None, titles=None, max_column_widths=None):
    if is_json_output() or is_yaml_output():
        new_rows = []
        for row in rows:
            new_row = {}
            for col in cols:
                new_row[col] = row.get(col)
            new_rows.append(new_row)
        if is_json_output():
            print(json.dumps(new_rows, sort_keys=True))
        else:
            print(yaml.safe_dump_all(new_rows, default_flow_style=False))
        return
    elif is_tsv_output():
        return print_tsv_table(cols, rows)

    if not styles or type(styles) != dict:
        styles = {}

    if not titles or type(titles) != dict:
        titles = {}

    if not max_column_widths or type(max_column_widths) != dict:
        max_column_widths = {}

    colwidths = {}

    for col in cols:
        colwidths[col] = len(titles.get(col, col))

    for row in rows:
        for col in cols:
            val = row.get(col)
            colwidths[col] = min(max(colwidths[col], len(format(col, val))), max_column_widths.get(col, 1000))

    for i, col in enumerate(cols):
        click.secho(('{:' + str(colwidths[col]) + '}').format(titles.get(col, col.title().replace('_', ' '))),
                    nl=False, fg='black', bg='white')
        if i < len(cols) - 1:
            click.secho('│', nl=False, fg='black', bg='white')
    click.echo('')

    for row in rows:
        for col in cols:
            val = row.get(col)
            align = ''
            try:
                style = styles.get(val, {})
            except:
                # val might not be hashable
                style = {}
            if val is not None and col.endswith('_time') and isinstance(val, numbers.Number):
                align = '>'
                diff = time.time() - val
                if diff < 900:
                    style = {'fg': 'green', 'bold': True}
                elif diff < 3600:
                    style = {'fg': 'green'}
            elif isinstance(val, int) or isinstance(val, float):
                align = '>'
            val = format(col, val)

            if len(val) > max_column_widths.get(col, 1000):
                val = val[:max_column_widths.get(col, 1000) - 2] + '..'
            click.secho(('{:' + align + str(colwidths[col]) + '}').format(val), nl=False, **style)
            click.echo(' ', nl=False)
        click.echo('')


def choice(prompt, options, default=None):
    """
    Ask to user to select one option and return it
    """
    stderr = True
    if sys.stdout.isatty():
        stderr = False

    click.secho(prompt, err=stderr)
    promptdefault = None
    for i, option in enumerate(options):
        if isinstance(option, tuple):
            value, label = option
        else:
            value = label = option
        if value == default:
            promptdefault = i + 1
        click.secho('{}) {}'.format(i + 1, label), err=stderr)
    while True:
        selection = click.prompt('Please select (1-{})'.format(len(options)),
                                 type=int, default=promptdefault, err=stderr)
        try:
            result = options[int(selection) - 1]
            if isinstance(result, tuple):
                value, label = result
            else:
                value = result
            return value
        except:
            pass


class AliasedGroup(click.Group):
    """
    Click group which allows using abbreviated commands
    """

    def get_command(self, ctx, cmd_name):
        rv = click.Group.get_command(self, ctx, cmd_name)
        if rv is not None:
            return rv
        matches = [x for x in self.list_commands(ctx)
                   if x.startswith(cmd_name)]
        if not matches:
            return None
        elif len(matches) == 1:
            return click.Group.get_command(self, ctx, matches[0])
        ctx.fail('Too many matches: %s' % ', '.join(sorted(matches)))


class FloatRange(click.types.FloatParamType):
    """A parameter that works similar to :data:`click.FLOAT` but restricts
    the value to fit into a range.  The default behavior is to fail if the
    value falls outside the range, but it can also be silently clamped
    between the two edges.
    """
    name = 'float range'

    def __init__(self, min=None, max=None, clamp=False):
        self.min = min
        self.max = max
        self.clamp = clamp

    def convert(self, value, param, ctx):
        rv = click.types.FloatParamType.convert(self, value, param, ctx)
        if self.clamp:
            if self.min is not None and rv < self.min:
                return self.min
            if self.max is not None and rv > self.max:
                return self.max
        if self.min is not None and rv < self.min or \
           self.max is not None and rv > self.max:
            if self.min is None:
                self.fail('%s is bigger than the maximum valid value '
                          '%s.' % (rv, self.max), param, ctx)
            elif self.max is None:
                self.fail('%s is smaller than the minimum valid value '
                          '%s.' % (rv, self.min), param, ctx)
            else:
                self.fail('%s is not in the valid range of %s to %s.'
                          % (rv, self.min, self.max), param, ctx)
        return rv

    def __repr__(self):
        return 'FloatRange(%r, %r)' % (self.min, self.max)


class UrlType(click.types.ParamType):
    name = 'url'

    def __init__(self, default_scheme='https', allowed_schemes=('http', 'https')):
        self.default_scheme = default_scheme
        self.allowed_schemes = allowed_schemes

    def convert(self, value, param, ctx):
        value = value.strip()
        if not value:
            self.fail('"{}" is not a valid URL'.format(value))
        if self.default_scheme and '://' not in value:
            value = '{}://{}'.format(self.default_scheme, value)
        url = urlsplit(value)
        if self.allowed_schemes and url.scheme not in self.allowed_schemes:
            self.fail('"{}" is not one of the allowed URL schemes ({})'.format(
                      url.scheme, ', '.join(self.allowed_schemes)))
        return urlunsplit(url)

    def __repr__(self):
        return 'UrlType(%r, %r)' % (self.default_scheme, self.allowed_schemes)
