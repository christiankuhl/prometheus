class Foo(object):
    def __init__(self):
        self.x = 1
        self.x[0] = "b"

def hello_world(name):
    x = 5
    print("Hello, world, greetings from ol' \"" + name + "\"!")
    return 3 * 4 + 5

if __name__ == "__main__":
    hello_world("me, myself and I")