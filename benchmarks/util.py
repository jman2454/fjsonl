import json
import numpy as np    

def frozendict(d):
    if type(d) != dict:
        return d
    return frozenset({ (k, frozendict(d[k])) for k in d })

# for now assume 32 bit precision always
# somehow we can distinguish later
def convert_floats(d):
    # if type(d) == float:
    #     return np.float32(d)
    if type(d) != dict:
        return d

    for k in d:
        d[k] = convert_floats(d[k])

def get_jsons(filename):
    with open(filename, 'r') as f:
        result = [ frozendict(convert_floats(json.loads(l))) for l in f.read().split('\n') if l ]
        return result
    
if __name__ == '__main__':
    fjsonls = get_jsons('./fjsonl.txt')
    rapids = get_jsons('./RapidJSON.txt')
    nlohmanns = get_jsons('./nlohmann::json.txt')

    assert set(rapids) == set(nlohmanns)
    assert set(fjsonls) == set(rapids)

    for i in range(len(fjsonls)):
        if fjsonls[i] != rapids[i]:
            print('FJsonl had:')
            print(fjsonls[i] - rapids[i])

            print('RapidJSON had:')
            print(rapids[i] - fjsonls[i])
            break