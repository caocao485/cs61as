def memoize(func):
    memoize_list={}
    def proc(x):
        if x in memoize_list:
            return memoize_list[x]
        else:
            t = func(x,proc)
            memoize_list[x] = t
            return memoize_list[x]
    return proc



