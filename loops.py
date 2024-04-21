
###循环去除一个数组里小于0的数
###两种循环方法 创建一个数组把小于0的数给剔除掉
##
def __loops_while():
    _arr_= list(range(-15,6))
    i = 0
    while i < len(_arr_):
        if _arr_[i] < 0:
            _arr_.pop(i)
        else:
            i += 1
    print(_arr_)

def __loops_for():
    ## for <va> in <sequence>
    _arr_ = list(range(-15, 6))
    for e in _arr_.copy():#copy为了避免index问题
        if e <0:
            _arr_.remove(e)
    print(_arr_)

for i in range(5):
    i = i*i
    print(i)





