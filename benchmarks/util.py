import json
    
def frozendict(d):
    if type(d) != dict:
        return d
    return frozenset({ (k, frozendict(d[k])) for k in d })

def get_jsons(filename):
    with open(filename, 'r') as f:
        result = [ frozendict(json.loads(l)) for l in f.read().split('\n') if l ]
        return result
    
if __name__ == '__main__':
    fjsonls = get_jsons('./fjsonl.txt')
    rapids = get_jsons('./RapidJSON.txt')
    nlohmanns = get_jsons('./nlohmann::json.txt')

    assert set(fjsonls) == set(rapids)
    assert set(rapids) == set(nlohmanns)

    # for i in range(len(fjsonls)):
    #     if fjsonls[i] != rapids[i]:
    #         print('FJsonl had:')
    #         print(fjsonls[i] - rapids[i])

    #         print('RapidJSON had:')
    #         print(rapids[i] - fjsonls[i])
    #         break