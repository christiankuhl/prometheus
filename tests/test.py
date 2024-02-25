# A comment

import numpy as np
from .. import something
from .test import foo as bar
from scipy.optimize import *
from collections import defaultdict
from requests.adapters import BaseAdapter as Base

class Foo(Base):
    def __init__(self):
        self.x = 1
        self.x[0] = "b"

def hello_world(name):
    x = 5
    print("Hello, world, greetings from ol' \"" + name + "\"!")
    return 3 * 4 + 5

if __name__ == "__main__":
    hello_world("me, myself and I")