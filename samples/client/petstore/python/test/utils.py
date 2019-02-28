# coding: utf-8

from datetime import date, datetime, timedelta, tzinfo


class TimeZone(tzinfo):
    def __init__(self, hours_offset):
        self.time_delta = timedelta(hours=hours_offset)
    def utcoffset(self, dt):
        return self.time_delta

exmple_simple = {
    'int': [
        2,
        -5,
        0,
        2147483647, # int32 max
        -2147483648, # int32 min
        9223372036854775807, # int64 max
        -9223372036854775808 # int64 min
    ],
    'str': ['مرحبا كيف حالك', 'fancy\nstring', 'hey there'],
    'float': [3.14159, -12.4, 0.0],
    'bool': [True, False],
    'None': [None],
    'date': [date(1000, 7, 31), date(1970, 1, 1), date(2396, 2, 29)],
    'datetime': [
        datetime(1000, 7, 31, 13, 32, 57, tzinfo=None),
        datetime(1970, 1, 1, 2, 12, 41, tzinfo=TimeZone(0)),
        datetime(2396, 2, 29, 23, 57, 6, tzinfo=TimeZone(-6))
    ]
}

example_list_monotypes = [
    [],
    [exmple_simple['int'][0]],
    exmple_simple['int'],
    [exmple_simple['str'][0]],
    exmple_simple['str'],
    [exmple_simple['float'][0]],
    exmple_simple['float'],
    [exmple_simple['bool'][0]],
    exmple_simple['bool'],
    [exmple_simple['date'][0]],
    exmple_simple['date'],
    [exmple_simple['datetime'][0]],
    exmple_simple['datetime']
]

example_dict_monotypes = [
    {},
    {exmple_simple['str'][0]: exmple_simple['str'][0]},
    {exmple_simple['str'][0]: exmple_simple['int'][0]},
    {exmple_simple['str'][0]: exmple_simple['float'][0]},
    {exmple_simple['str'][0]: exmple_simple['bool'][0]},
    {exmple_simple['str'][0]: exmple_simple['date'][0]},
    {exmple_simple['str'][0]: exmple_simple['datetime'][0]},
    {exmple_simple['str'][1]: exmple_simple['str'][1]},
    {exmple_simple['str'][1]: exmple_simple['int'][1]},
    {exmple_simple['str'][1]: exmple_simple['float'][1]},
    {exmple_simple['str'][1]: exmple_simple['bool'][1]},
    {exmple_simple['str'][1]: exmple_simple['date'][1]},
    {exmple_simple['str'][1]: exmple_simple['datetime'][1]},
    {exmple_simple['str'][2]: exmple_simple['str'][2]},
    {exmple_simple['str'][2]: exmple_simple['int'][2]},
    {exmple_simple['str'][2]: exmple_simple['float'][2]},
    {exmple_simple['str'][2]: exmple_simple['bool'][0]},
    {exmple_simple['str'][2]: exmple_simple['date'][2]},
    {exmple_simple['str'][2]: exmple_simple['datetime'][2]}
]
# add lists to dict
example_dict_monotypes_all = list(example_dict_monotypes)
for example_list_monotype in example_list_monotypes:
    example_dict_monotypes_all.append(
        {exmple_simple['str'][0]: example_list_monotype}
    )
# add self to dict, nested
current_items = list(example_dict_monotypes_all)
for current_item in current_items:
    example_dict_monotypes_all.append(
        {exmple_simple['str'][0]: current_item}
    )

# add dicts to list
example_list_monotypes_all = list(example_list_monotypes)
for example_dict_monotype in example_dict_monotypes:
    example_list_monotypes_all.append(
        [example_dict_monotype]
    )
# add self to list, nested
current_items = list(example_list_monotypes_all)
for current_item in current_items:
    example_list_monotypes_all.append(
        [current_item]
    )
EXAMPLES = {
    'int': exmple_simple['int'],
    'str': exmple_simple['str'],
    'float': exmple_simple['float'],
    'bool': exmple_simple['bool'],
    'None': exmple_simple['None'],
    'date': exmple_simple['date'],
    'datetime': exmple_simple['datetime'],
    'list': example_list_monotypes_all,
    'dict': example_dict_monotypes_all
}

VAR_NAME_INVALID_TYPES = ['int', 'float', 'bool', 'None']


def get_examples(types):
    res = []
    for type in types:
        res.extend(EXAMPLES[type])
    return res
