def foo1_impl():
    a = 40
    b = 2
    return a + b

def foo1():
    return foo1_impl()

def foo2_impl(a, b):
    return a + b

def foo2():
    a = 40
    b = 2
    return foo2_impl(a, b)

print(foo1())
print(foo2())