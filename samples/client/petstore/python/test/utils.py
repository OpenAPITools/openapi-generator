import copy

exmple_simple = {
    'int': [2, -5, 0],
    'str': ['مرحبا كيف حالك', 'fancy\nstring', 'hey there'],
    'float': [3.14159, -12.4, 0.0],
    'bool': [True, False],
    'None': [None]
}
example_list_monotypes = [
    [],
    [exmple_simple['int'][0]],
    [exmple_simple['int'][0], exmple_simple['int'][1]],
    [exmple_simple['str'][0]],
    [exmple_simple['str'][0], exmple_simple['str'][1]],
    [exmple_simple['float'][0]],
    [exmple_simple['float'][0], exmple_simple['float'][1]],
    [exmple_simple['bool'][0]],
    [exmple_simple['bool'][0], exmple_simple['bool'][1]]
]

example_dict_monotypes = [
    {},
    {exmple_simple['str'][0]: exmple_simple['str'][1]},
    {exmple_simple['str'][0]: exmple_simple['int'][0]},
    {exmple_simple['str'][0]: exmple_simple['float'][0]},
    {exmple_simple['str'][0]: exmple_simple['bool'][0]},
    {exmple_simple['str'][1]: exmple_simple['str'][1]},
    {exmple_simple['str'][1]: exmple_simple['int'][0]},
    {exmple_simple['str'][1]: exmple_simple['float'][0]},
    {exmple_simple['str'][1]: exmple_simple['bool'][0]}
]
# add lists to dict
example_dict_monotypes_all = copy.deepcopy(example_dict_monotypes)
for example_list_monotype in example_list_monotypes:
    example_dict_monotypes_all.append(
        {exmple_simple['str'][0]: example_list_monotype}
    )
# add self to dict, nested
current_items = copy.deepcopy(example_dict_monotypes_all)
for current_item in current_items:
    example_dict_monotypes_all.append(
        {exmple_simple['str'][0]: current_item}
    )

# add dicts to list
example_list_monotypes_all = copy.deepcopy(example_list_monotypes)
for example_dict_monotype in example_dict_monotypes:
    example_list_monotypes_all.append(
        [example_dict_monotype]
    )
# add self to list, nested
current_items = copy.deepcopy(example_list_monotypes_all)
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
    'list': example_list_monotypes_all,
    'dict': example_dict_monotypes_all
}

VAR_NAME_INVALID_TYPES = ['int', 'float', 'bool', 'None']


def get_examples(types):
    res = []
    for type in types:
        res.extend(EXAMPLES[type])
    return res
