class Value:
    def __init__(self, inner):
        self.inner = inner

    def __str__(self):
        return f'Value({self.inner})'


class Linked:
    def __init__(self, head, tail):
        self.head = head
        self.tail = tail

    @classmethod
    def cons(cls, head, tail):
        return Value(cls(head, tail))

    @classmethod
    def nil(cls):
        return Value(None)

    @staticmethod
    def append(linked1, linked2):
        if linked1.inner is None:
            linked1.inner = linked2.inner
        else:
            Linked.append(linked1.inner.tail, linked2)

    items = []

    def __str__(self):
        if self in self.items:
            return "[...]"
        self.items.append(self)
        s = f'{self.head} : {self.tail}'
        self.items.pop()
        return s


xs = Linked.cons(42, Linked.nil())
print(xs)
ys = Linked.cons(43, Linked.nil())
Linked.append(xs, ys)
print(xs)
Linked.append(xs, xs)
print(xs)