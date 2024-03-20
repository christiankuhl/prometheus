#!/usr/bin/python3

from cardgames import Card, Hand, Deck
from itertools import cycle, zip_longest
import time
import os

class Player(object):
    def __init__(self, game, name):
        self.game = game
        self.name = name
        self.cards = Hand()
    def get_card(self):
        self.cards.append(self.game.deck.pop())

class Pointeur(Player):
    def __repr__(self):
        return self.cards.repr(style="vertical")

class Croupier(Player):
    def __init__(self, game, name):
        super().__init__(game, name)
        self.cards = Hand(maxwidth=11)
    def __repr__(self):
        return self.cards.repr(style="horizontal")

class HumanPlayer(Pointeur):
    def __init__(self, game, name):
        super().__init__(game, name)

class AIPlayer(Pointeur):
    def __init__(self, game, name):
        super().__init__(game, name)

class Game(object):
    def __init__(self, players=3):
        self.deck = Deck(n=6, start=2)
        self.croupier = Croupier(self, "")
        self.player_list = [AIPlayer(self, "") for _ in range(players)] + [self.croupier]
        self.players = cycle(self.player_list)
        for _ in range(2):
            for _ in range(players + 1):
                time.sleep(1)
                self.current_player = next(self.players)
                self.current_player.get_card()
                print(self)
    def __repr__(self):
        anchor = "\x1b7\x1b[1;1f" # ANSI escape sequence to start at row 1, columm 1
        width = 13 * (len(self.player_list) - 1) - 5
        top = "\n".join(map(lambda arg: "{:^{width}}".format(arg, width=width), str(self.croupier).splitlines()))
        bottom = "\n".join(map((" " * 5).join, zip_longest(*[str(p).splitlines() for p in self.player_list[:-1]], fillvalue=" " * 7)))
        return anchor + top + "\n" * 5 + bottom + "\x1b8"

if __name__ == "__main__":
    os.system("clear")
    g = Game()
from itertools import product
from random import shuffle

class Card(object):
    """
    A Card is made up of a suite and a rank, and a fancy unicode __repr__ string.
    """
    symbols = {"spades": u"\u2660",
               "hearts": u"\u2665",
               "diamonds": u"\u2666",
               "clubs": u"\u2663"}
    def __init__(self, rank, suite):
        self.rank = rank
        self.suite = suite
    def __repr__(self):
        """
        Paint a card with the rank in the top left and bottom right corner
        and the suite symbol printed in the middle.
        """
        bottom = u"\u2570"+ u"\u2500"*5 + u"\u256f"
        if len(self.rank) < 3:
            rank = self.rank
        else:
            rank = self.rank[0].upper()
        top = u"\u256d" + u"\u2500"*5 + u"\u256e\n"
        interior = (u"\u2502" + "{:<2}".format(rank) + " " * 3 + u"\u2502\n"
                    + u"\u2502" + "     " + u"\u2502\n"
                    + u"\u2502" + "  " + Card.symbols[self.suite] + "  " + u"\u2502\n"
                    + u"\u2502" + "     " + u"\u2502\n"
                    + u"\u2502" + " " * 3 + "{:>2}".format(rank) + u"\u2502\n")
        return top + interior + bottom
    HIDDEN = u"\u256d" + u"\u2500"*5 + u"\u256e\n"  + (u"\u2502" + "/////" + u"\u2502\n") * 5 + u"\u2570"+ u"\u2500"*5 + u"\u256f"

class Hand(list):
    """
    A Hand() object is just a list of Card() objects, which additionally can be
    addressed by an alphabetical index in order for the user to be able to
    select a card with a single key press.
    """
    alphabet = "123456789abcdefghijklmnopqrstuvwxyz"
    def __init__(self, *args, style="horizontal", name="", maxwidth=59, **kwargs):
        self.style = style
        self.name = name
        self.maxwidth = maxwidth
        super().__init__(*args, **kwargs)
    def __call__(self, index):
        position = Hand.alphabet.find(index.lower())
        if position > -1:
            return self[position]
        else:
            raise IndexError
    # def repr(self, style="horizontal", hide=["1"], card_width=8):
    #     cards = []
    #     for index, card in zip(Hand.alphabet, self):
    #         if index in hide:
    #             cards.append(Card.HIDDEN)
    #         else:
    #             cards.append(str(card))
    #     if style == "horizontal":
    #         try:
    #             repr_string = "Foo"
    #             # repr_string = "\n".join(map(lambda arg: "{:<{maxwidth}}".format(arg, maxwidth=self.maxwidth),
    #             #                      ["".join([card.split("\n")[index][:card_width] + " " * max(card_width - 7, 0)
    #             #                                 for card in cards[:-1]])
    #             #                         + cards[-1].split("\n")[index]
    #             #                                         for index in range(7)]))
    #         except IndexError:
    #             # No cards to be displayed
    #             repr_string = "\n".join([" " * self.maxwidth] * 7)
    #     elif style == "vertical":
    #         try:
    #             repr_string = "\n".join([l for c in cards[:-1] for l in str(c).split("\n")[:4]]) + "\n" + cards[-1]
    #         except IndexError:
    #             repr_string = "\n".join([" " * 7] * 7)
    #     elif style == "hidden":
    #         # Print a single box with the player's name and the number of cards in the
    #         # player's hand in it.
    #         top = u"\u256d" + u"\u2500"*5 + u"\u256e\n"
    #         bottom = u"\u2570"+ u"\u2500"*5 + u"\u256f"
            # interior = (u"\u2502" + u"/" * 5 + u"\u2502\n"
            #             + u"\u2502" + "{:/^5}".format(self.name) + u"\u2502\n"
            #             + u"\u2502" + "/" + "{:/>2}".format(len(self)) + "//" + u"\u2502\n"
            #             + (u"\u2502" + "/" * 5 + u"\u2502\n") * 2 )
    #         repr_string = top + interior + bottom
    #     elif style == "top":
    #         repr_string = str(self[-1])
    #     return repr_string
    # def __repr__(self):
    #     return self.repr(self.style)

# class Deck(list):
#     """
#     A Deck(n, s) is a shuffeled list of n copies of the cartesian product of
#     Deck.suites and ranks from s to 10 and "jack", "queen", "king" and "ace".
#     """
#     suites = ["spades", "hearts", "diamonds", "clubs"]
#     def __init__(self, n=1, start=7):
#         ranks = [str(k) for k in range(start, 11)] + ["jack", "queen", "king", "ace"]
#         base_deck = list(map(lambda c: Card(*c), list(product(ranks, Deck.suites)))) * n
#         shuffle(base_deck)
#         super().__init__(base_deck)
print("Hello, world!")

lambda: None
lambda x: x**2
lambda x, **y: x**2 + y

f(f(f(f(f(f(f(f(f(g(h(i(j(k(x))))))))))))))

f"abc"
u"def"
r"raw"
fr"abc"# A comment

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
    
for _ in range(3):
    print("Foo")

#!/usr/bin/python3

from cardgames import Card, Hand, Deck
from itertools import cycle, zip_longest
import time
import os

class Player(object):
    def __init__(self, game, name):
        self.game = game
        self.name = name
        self.cards = Hand()
    def get_card(self):
        self.cards.append(self.game.deck.pop())

class Pointeur(Player):
    def __repr__(self):
        return self.cards.repr(style="vertical")

class Croupier(Player):
    def __init__(self, game, name):
        super().__init__(game, name)
        self.cards = Hand(maxwidth=11)
    def __repr__(self):
        return self.cards.repr(style="horizontal")

class HumanPlayer(Pointeur):
    def __init__(self, game, name):
        super().__init__(game, name)

class AIPlayer(Pointeur):
    def __init__(self, game, name):
        super().__init__(game, name)

class Game(object):
    def __init__(self, players=3):
        self.deck = Deck(n=6, start=2)
        self.croupier = Croupier(self, "")
        self.player_list = [AIPlayer(self, "") for _ in range(players)] + [self.croupier]
        self.players = cycle(self.player_list)
        for _ in range(2):
            for _ in range(players + 1):
                time.sleep(1)
                self.current_player = next(self.players)
                self.current_player.get_card()
                print(self)
    def __repr__(self):
        anchor = "\x1b7\x1b[1;1f" # ANSI escape sequence to start at row 1, columm 1
        width = 13 * (len(self.player_list) - 1) - 5
        top = "\n".join(map(lambda arg: "{:^{width}}".format(arg, width=width), str(self.croupier).splitlines()))
        bottom = "\n".join(map((" " * 5).join, zip_longest(*[str(p).splitlines() for p in self.player_list[:-1]], fillvalue=" " * 7)))
        return anchor + top + "\n" * 5 + bottom + "\x1b8"

if __name__ == "__main__":
    os.system("clear")
    g = Game()
from itertools import product
from random import shuffle

class Card(object):
    """
    A Card is made up of a suite and a rank, and a fancy unicode __repr__ string.
    """
    symbols = {"spades": u"\u2660",
               "hearts": u"\u2665",
               "diamonds": u"\u2666",
               "clubs": u"\u2663"}
    def __init__(self, rank, suite):
        self.rank = rank
        self.suite = suite
    def __repr__(self):
        """
        Paint a card with the rank in the top left and bottom right corner
        and the suite symbol printed in the middle.
        """
        bottom = u"\u2570"+ u"\u2500"*5 + u"\u256f"
        if len(self.rank) < 3:
            rank = self.rank
        else:
            rank = self.rank[0].upper()
        top = u"\u256d" + u"\u2500"*5 + u"\u256e\n"
        interior = (u"\u2502" + "{:<2}".format(rank) + " " * 3 + u"\u2502\n"
                    + u"\u2502" + "     " + u"\u2502\n"
                    + u"\u2502" + "  " + Card.symbols[self.suite] + "  " + u"\u2502\n"
                    + u"\u2502" + "     " + u"\u2502\n"
                    + u"\u2502" + " " * 3 + "{:>2}".format(rank) + u"\u2502\n")
        return top + interior + bottom
    HIDDEN = u"\u256d" + u"\u2500"*5 + u"\u256e\n"  + (u"\u2502" + "/////" + u"\u2502\n") * 5 + u"\u2570"+ u"\u2500"*5 + u"\u256f"

class Hand(list):
    """
    A Hand() object is just a list of Card() objects, which additionally can be
    addressed by an alphabetical index in order for the user to be able to
    select a card with a single key press.
    """
    alphabet = "123456789abcdefghijklmnopqrstuvwxyz"
    def __init__(self, *args, style="horizontal", name="", maxwidth=59, **kwargs):
        self.style = style
        self.name = name
        self.maxwidth = maxwidth
        super().__init__(*args, **kwargs)
    def __call__(self, index):
        position = Hand.alphabet.find(index.lower())
        if position > -1:
            return self[position]
        else:
            raise IndexError
    # def repr(self, style="horizontal", hide=["1"], card_width=8):
    #     cards = []
    #     for index, card in zip(Hand.alphabet, self):
    #         if index in hide:
    #             cards.append(Card.HIDDEN)
    #         else:
    #             cards.append(str(card))
    #     if style == "horizontal":
    #         try:
    #             repr_string = "Foo"
    #             # repr_string = "\n".join(map(lambda arg: "{:<{maxwidth}}".format(arg, maxwidth=self.maxwidth),
    #             #                      ["".join([card.split("\n")[index][:card_width] + " " * max(card_width - 7, 0)
    #             #                                 for card in cards[:-1]])
    #             #                         + cards[-1].split("\n")[index]
    #             #                                         for index in range(7)]))
    #         except IndexError:
    #             # No cards to be displayed
    #             repr_string = "\n".join([" " * self.maxwidth] * 7)
    #     elif style == "vertical":
    #         try:
    #             repr_string = "\n".join([l for c in cards[:-1] for l in str(c).split("\n")[:4]]) + "\n" + cards[-1]
    #         except IndexError:
    #             repr_string = "\n".join([" " * 7] * 7)
    #     elif style == "hidden":
    #         # Print a single box with the player's name and the number of cards in the
    #         # player's hand in it.
    #         top = u"\u256d" + u"\u2500"*5 + u"\u256e\n"
    #         bottom = u"\u2570"+ u"\u2500"*5 + u"\u256f"
            # interior = (u"\u2502" + u"/" * 5 + u"\u2502\n"
            #             + u"\u2502" + "{:/^5}".format(self.name) + u"\u2502\n"
            #             + u"\u2502" + "/" + "{:/>2}".format(len(self)) + "//" + u"\u2502\n"
            #             + (u"\u2502" + "/" * 5 + u"\u2502\n") * 2 )
    #         repr_string = top + interior + bottom
    #     elif style == "top":
    #         repr_string = str(self[-1])
    #     return repr_string
    # def __repr__(self):
    #     return self.repr(self.style)

# class Deck(list):
#     """
#     A Deck(n, s) is a shuffeled list of n copies of the cartesian product of
#     Deck.suites and ranks from s to 10 and "jack", "queen", "king" and "ace".
#     """
#     suites = ["spades", "hearts", "diamonds", "clubs"]
#     def __init__(self, n=1, start=7):
#         ranks = [str(k) for k in range(start, 11)] + ["jack", "queen", "king", "ace"]
#         base_deck = list(map(lambda c: Card(*c), list(product(ranks, Deck.suites)))) * n
#         shuffle(base_deck)
#         super().__init__(base_deck)
print("Hello, world!")

lambda: None
lambda x: x**2
lambda x, **y: x**2 + y

f(f(f(f(f(f(f(f(f(g(h(i(j(k(x))))))))))))))

f"abc"
u"def"
r"raw"
fr"abc"# A comment

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
    
for _ in range(3):
    print("Foo")

#!/usr/bin/python3

from cardgames import Card, Hand, Deck
from itertools import cycle, zip_longest
import time
import os

class Player(object):
    def __init__(self, game, name):
        self.game = game
        self.name = name
        self.cards = Hand()
    def get_card(self):
        self.cards.append(self.game.deck.pop())

class Pointeur(Player):
    def __repr__(self):
        return self.cards.repr(style="vertical")

class Croupier(Player):
    def __init__(self, game, name):
        super().__init__(game, name)
        self.cards = Hand(maxwidth=11)
    def __repr__(self):
        return self.cards.repr(style="horizontal")

class HumanPlayer(Pointeur):
    def __init__(self, game, name):
        super().__init__(game, name)

class AIPlayer(Pointeur):
    def __init__(self, game, name):
        super().__init__(game, name)

class Game(object):
    def __init__(self, players=3):
        self.deck = Deck(n=6, start=2)
        self.croupier = Croupier(self, "")
        self.player_list = [AIPlayer(self, "") for _ in range(players)] + [self.croupier]
        self.players = cycle(self.player_list)
        for _ in range(2):
            for _ in range(players + 1):
                time.sleep(1)
                self.current_player = next(self.players)
                self.current_player.get_card()
                print(self)
    def __repr__(self):
        anchor = "\x1b7\x1b[1;1f" # ANSI escape sequence to start at row 1, columm 1
        width = 13 * (len(self.player_list) - 1) - 5
        top = "\n".join(map(lambda arg: "{:^{width}}".format(arg, width=width), str(self.croupier).splitlines()))
        bottom = "\n".join(map((" " * 5).join, zip_longest(*[str(p).splitlines() for p in self.player_list[:-1]], fillvalue=" " * 7)))
        return anchor + top + "\n" * 5 + bottom + "\x1b8"

if __name__ == "__main__":
    os.system("clear")
    g = Game()
from itertools import product
from random import shuffle

class Card(object):
    """
    A Card is made up of a suite and a rank, and a fancy unicode __repr__ string.
    """
    symbols = {"spades": u"\u2660",
               "hearts": u"\u2665",
               "diamonds": u"\u2666",
               "clubs": u"\u2663"}
    def __init__(self, rank, suite):
        self.rank = rank
        self.suite = suite
    def __repr__(self):
        """
        Paint a card with the rank in the top left and bottom right corner
        and the suite symbol printed in the middle.
        """
        bottom = u"\u2570"+ u"\u2500"*5 + u"\u256f"
        if len(self.rank) < 3:
            rank = self.rank
        else:
            rank = self.rank[0].upper()
        top = u"\u256d" + u"\u2500"*5 + u"\u256e\n"
        interior = (u"\u2502" + "{:<2}".format(rank) + " " * 3 + u"\u2502\n"
                    + u"\u2502" + "     " + u"\u2502\n"
                    + u"\u2502" + "  " + Card.symbols[self.suite] + "  " + u"\u2502\n"
                    + u"\u2502" + "     " + u"\u2502\n"
                    + u"\u2502" + " " * 3 + "{:>2}".format(rank) + u"\u2502\n")
        return top + interior + bottom
    HIDDEN = u"\u256d" + u"\u2500"*5 + u"\u256e\n"  + (u"\u2502" + "/////" + u"\u2502\n") * 5 + u"\u2570"+ u"\u2500"*5 + u"\u256f"

class Hand(list):
    """
    A Hand() object is just a list of Card() objects, which additionally can be
    addressed by an alphabetical index in order for the user to be able to
    select a card with a single key press.
    """
    alphabet = "123456789abcdefghijklmnopqrstuvwxyz"
    def __init__(self, *args, style="horizontal", name="", maxwidth=59, **kwargs):
        self.style = style
        self.name = name
        self.maxwidth = maxwidth
        super().__init__(*args, **kwargs)
    def __call__(self, index):
        position = Hand.alphabet.find(index.lower())
        if position > -1:
            return self[position]
        else:
            raise IndexError
    # def repr(self, style="horizontal", hide=["1"], card_width=8):
    #     cards = []
    #     for index, card in zip(Hand.alphabet, self):
    #         if index in hide:
    #             cards.append(Card.HIDDEN)
    #         else:
    #             cards.append(str(card))
    #     if style == "horizontal":
    #         try:
    #             repr_string = "Foo"
    #             # repr_string = "\n".join(map(lambda arg: "{:<{maxwidth}}".format(arg, maxwidth=self.maxwidth),
    #             #                      ["".join([card.split("\n")[index][:card_width] + " " * max(card_width - 7, 0)
    #             #                                 for card in cards[:-1]])
    #             #                         + cards[-1].split("\n")[index]
    #             #                                         for index in range(7)]))
    #         except IndexError:
    #             # No cards to be displayed
    #             repr_string = "\n".join([" " * self.maxwidth] * 7)
    #     elif style == "vertical":
    #         try:
    #             repr_string = "\n".join([l for c in cards[:-1] for l in str(c).split("\n")[:4]]) + "\n" + cards[-1]
    #         except IndexError:
    #             repr_string = "\n".join([" " * 7] * 7)
    #     elif style == "hidden":
    #         # Print a single box with the player's name and the number of cards in the
    #         # player's hand in it.
    #         top = u"\u256d" + u"\u2500"*5 + u"\u256e\n"
    #         bottom = u"\u2570"+ u"\u2500"*5 + u"\u256f"
            # interior = (u"\u2502" + u"/" * 5 + u"\u2502\n"
            #             + u"\u2502" + "{:/^5}".format(self.name) + u"\u2502\n"
            #             + u"\u2502" + "/" + "{:/>2}".format(len(self)) + "//" + u"\u2502\n"
            #             + (u"\u2502" + "/" * 5 + u"\u2502\n") * 2 )
    #         repr_string = top + interior + bottom
    #     elif style == "top":
    #         repr_string = str(self[-1])
    #     return repr_string
    # def __repr__(self):
    #     return self.repr(self.style)

# class Deck(list):
#     """
#     A Deck(n, s) is a shuffeled list of n copies of the cartesian product of
#     Deck.suites and ranks from s to 10 and "jack", "queen", "king" and "ace".
#     """
#     suites = ["spades", "hearts", "diamonds", "clubs"]
#     def __init__(self, n=1, start=7):
#         ranks = [str(k) for k in range(start, 11)] + ["jack", "queen", "king", "ace"]
#         base_deck = list(map(lambda c: Card(*c), list(product(ranks, Deck.suites)))) * n
#         shuffle(base_deck)
#         super().__init__(base_deck)
print("Hello, world!")

lambda: None
lambda x: x**2
lambda x, **y: x**2 + y

f(f(f(f(f(f(f(f(f(g(h(i(j(k(x))))))))))))))

f"abc"
u"def"
r"raw"
fr"abc"# A comment

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
    
for _ in range(3):
    print("Foo")

#!/usr/bin/python3

from cardgames import Card, Hand, Deck
from itertools import cycle, zip_longest
import time
import os

class Player(object):
    def __init__(self, game, name):
        self.game = game
        self.name = name
        self.cards = Hand()
    def get_card(self):
        self.cards.append(self.game.deck.pop())

class Pointeur(Player):
    def __repr__(self):
        return self.cards.repr(style="vertical")

class Croupier(Player):
    def __init__(self, game, name):
        super().__init__(game, name)
        self.cards = Hand(maxwidth=11)
    def __repr__(self):
        return self.cards.repr(style="horizontal")

class HumanPlayer(Pointeur):
    def __init__(self, game, name):
        super().__init__(game, name)

class AIPlayer(Pointeur):
    def __init__(self, game, name):
        super().__init__(game, name)

class Game(object):
    def __init__(self, players=3):
        self.deck = Deck(n=6, start=2)
        self.croupier = Croupier(self, "")
        self.player_list = [AIPlayer(self, "") for _ in range(players)] + [self.croupier]
        self.players = cycle(self.player_list)
        for _ in range(2):
            for _ in range(players + 1):
                time.sleep(1)
                self.current_player = next(self.players)
                self.current_player.get_card()
                print(self)
    def __repr__(self):
        anchor = "\x1b7\x1b[1;1f" # ANSI escape sequence to start at row 1, columm 1
        width = 13 * (len(self.player_list) - 1) - 5
        top = "\n".join(map(lambda arg: "{:^{width}}".format(arg, width=width), str(self.croupier).splitlines()))
        bottom = "\n".join(map((" " * 5).join, zip_longest(*[str(p).splitlines() for p in self.player_list[:-1]], fillvalue=" " * 7)))
        return anchor + top + "\n" * 5 + bottom + "\x1b8"

if __name__ == "__main__":
    os.system("clear")
    g = Game()
from itertools import product
from random import shuffle

class Card(object):
    """
    A Card is made up of a suite and a rank, and a fancy unicode __repr__ string.
    """
    symbols = {"spades": u"\u2660",
               "hearts": u"\u2665",
               "diamonds": u"\u2666",
               "clubs": u"\u2663"}
    def __init__(self, rank, suite):
        self.rank = rank
        self.suite = suite
    def __repr__(self):
        """
        Paint a card with the rank in the top left and bottom right corner
        and the suite symbol printed in the middle.
        """
        bottom = u"\u2570"+ u"\u2500"*5 + u"\u256f"
        if len(self.rank) < 3:
            rank = self.rank
        else:
            rank = self.rank[0].upper()
        top = u"\u256d" + u"\u2500"*5 + u"\u256e\n"
        interior = (u"\u2502" + "{:<2}".format(rank) + " " * 3 + u"\u2502\n"
                    + u"\u2502" + "     " + u"\u2502\n"
                    + u"\u2502" + "  " + Card.symbols[self.suite] + "  " + u"\u2502\n"
                    + u"\u2502" + "     " + u"\u2502\n"
                    + u"\u2502" + " " * 3 + "{:>2}".format(rank) + u"\u2502\n")
        return top + interior + bottom
    HIDDEN = u"\u256d" + u"\u2500"*5 + u"\u256e\n"  + (u"\u2502" + "/////" + u"\u2502\n") * 5 + u"\u2570"+ u"\u2500"*5 + u"\u256f"

class Hand(list):
    """
    A Hand() object is just a list of Card() objects, which additionally can be
    addressed by an alphabetical index in order for the user to be able to
    select a card with a single key press.
    """
    alphabet = "123456789abcdefghijklmnopqrstuvwxyz"
    def __init__(self, *args, style="horizontal", name="", maxwidth=59, **kwargs):
        self.style = style
        self.name = name
        self.maxwidth = maxwidth
        super().__init__(*args, **kwargs)
    def __call__(self, index):
        position = Hand.alphabet.find(index.lower())
        if position > -1:
            return self[position]
        else:
            raise IndexError
    # def repr(self, style="horizontal", hide=["1"], card_width=8):
    #     cards = []
    #     for index, card in zip(Hand.alphabet, self):
    #         if index in hide:
    #             cards.append(Card.HIDDEN)
    #         else:
    #             cards.append(str(card))
    #     if style == "horizontal":
    #         try:
    #             repr_string = "Foo"
    #             # repr_string = "\n".join(map(lambda arg: "{:<{maxwidth}}".format(arg, maxwidth=self.maxwidth),
    #             #                      ["".join([card.split("\n")[index][:card_width] + " " * max(card_width - 7, 0)
    #             #                                 for card in cards[:-1]])
    #             #                         + cards[-1].split("\n")[index]
    #             #                                         for index in range(7)]))
    #         except IndexError:
    #             # No cards to be displayed
    #             repr_string = "\n".join([" " * self.maxwidth] * 7)
    #     elif style == "vertical":
    #         try:
    #             repr_string = "\n".join([l for c in cards[:-1] for l in str(c).split("\n")[:4]]) + "\n" + cards[-1]
    #         except IndexError:
    #             repr_string = "\n".join([" " * 7] * 7)
    #     elif style == "hidden":
    #         # Print a single box with the player's name and the number of cards in the
    #         # player's hand in it.
    #         top = u"\u256d" + u"\u2500"*5 + u"\u256e\n"
    #         bottom = u"\u2570"+ u"\u2500"*5 + u"\u256f"
            # interior = (u"\u2502" + u"/" * 5 + u"\u2502\n"
            #             + u"\u2502" + "{:/^5}".format(self.name) + u"\u2502\n"
            #             + u"\u2502" + "/" + "{:/>2}".format(len(self)) + "//" + u"\u2502\n"
            #             + (u"\u2502" + "/" * 5 + u"\u2502\n") * 2 )
    #         repr_string = top + interior + bottom
    #     elif style == "top":
    #         repr_string = str(self[-1])
    #     return repr_string
    # def __repr__(self):
    #     return self.repr(self.style)

# class Deck(list):
#     """
#     A Deck(n, s) is a shuffeled list of n copies of the cartesian product of
#     Deck.suites and ranks from s to 10 and "jack", "queen", "king" and "ace".
#     """
#     suites = ["spades", "hearts", "diamonds", "clubs"]
#     def __init__(self, n=1, start=7):
#         ranks = [str(k) for k in range(start, 11)] + ["jack", "queen", "king", "ace"]
#         base_deck = list(map(lambda c: Card(*c), list(product(ranks, Deck.suites)))) * n
#         shuffle(base_deck)
#         super().__init__(base_deck)
print("Hello, world!")

lambda: None
lambda x: x**2
lambda x, **y: x**2 + y

f(f(f(f(f(f(f(f(f(g(h(i(j(k(x))))))))))))))

f"abc"
u"def"
r"raw"
fr"abc"# A comment

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
    
for _ in range(3):
    print("Foo")

#!/usr/bin/python3

from cardgames import Card, Hand, Deck
from itertools import cycle, zip_longest
import time
import os

class Player(object):
    def __init__(self, game, name):
        self.game = game
        self.name = name
        self.cards = Hand()
    def get_card(self):
        self.cards.append(self.game.deck.pop())

class Pointeur(Player):
    def __repr__(self):
        return self.cards.repr(style="vertical")

class Croupier(Player):
    def __init__(self, game, name):
        super().__init__(game, name)
        self.cards = Hand(maxwidth=11)
    def __repr__(self):
        return self.cards.repr(style="horizontal")

class HumanPlayer(Pointeur):
    def __init__(self, game, name):
        super().__init__(game, name)

class AIPlayer(Pointeur):
    def __init__(self, game, name):
        super().__init__(game, name)

class Game(object):
    def __init__(self, players=3):
        self.deck = Deck(n=6, start=2)
        self.croupier = Croupier(self, "")
        self.player_list = [AIPlayer(self, "") for _ in range(players)] + [self.croupier]
        self.players = cycle(self.player_list)
        for _ in range(2):
            for _ in range(players + 1):
                time.sleep(1)
                self.current_player = next(self.players)
                self.current_player.get_card()
                print(self)
    def __repr__(self):
        anchor = "\x1b7\x1b[1;1f" # ANSI escape sequence to start at row 1, columm 1
        width = 13 * (len(self.player_list) - 1) - 5
        top = "\n".join(map(lambda arg: "{:^{width}}".format(arg, width=width), str(self.croupier).splitlines()))
        bottom = "\n".join(map((" " * 5).join, zip_longest(*[str(p).splitlines() for p in self.player_list[:-1]], fillvalue=" " * 7)))
        return anchor + top + "\n" * 5 + bottom + "\x1b8"

if __name__ == "__main__":
    os.system("clear")
    g = Game()
from itertools import product
from random import shuffle

class Card(object):
    """
    A Card is made up of a suite and a rank, and a fancy unicode __repr__ string.
    """
    symbols = {"spades": u"\u2660",
               "hearts": u"\u2665",
               "diamonds": u"\u2666",
               "clubs": u"\u2663"}
    def __init__(self, rank, suite):
        self.rank = rank
        self.suite = suite
    def __repr__(self):
        """
        Paint a card with the rank in the top left and bottom right corner
        and the suite symbol printed in the middle.
        """
        bottom = u"\u2570"+ u"\u2500"*5 + u"\u256f"
        if len(self.rank) < 3:
            rank = self.rank
        else:
            rank = self.rank[0].upper()
        top = u"\u256d" + u"\u2500"*5 + u"\u256e\n"
        interior = (u"\u2502" + "{:<2}".format(rank) + " " * 3 + u"\u2502\n"
                    + u"\u2502" + "     " + u"\u2502\n"
                    + u"\u2502" + "  " + Card.symbols[self.suite] + "  " + u"\u2502\n"
                    + u"\u2502" + "     " + u"\u2502\n"
                    + u"\u2502" + " " * 3 + "{:>2}".format(rank) + u"\u2502\n")
        return top + interior + bottom
    HIDDEN = u"\u256d" + u"\u2500"*5 + u"\u256e\n"  + (u"\u2502" + "/////" + u"\u2502\n") * 5 + u"\u2570"+ u"\u2500"*5 + u"\u256f"

class Hand(list):
    """
    A Hand() object is just a list of Card() objects, which additionally can be
    addressed by an alphabetical index in order for the user to be able to
    select a card with a single key press.
    """
    alphabet = "123456789abcdefghijklmnopqrstuvwxyz"
    def __init__(self, *args, style="horizontal", name="", maxwidth=59, **kwargs):
        self.style = style
        self.name = name
        self.maxwidth = maxwidth
        super().__init__(*args, **kwargs)
    def __call__(self, index):
        position = Hand.alphabet.find(index.lower())
        if position > -1:
            return self[position]
        else:
            raise IndexError
    # def repr(self, style="horizontal", hide=["1"], card_width=8):
    #     cards = []
    #     for index, card in zip(Hand.alphabet, self):
    #         if index in hide:
    #             cards.append(Card.HIDDEN)
    #         else:
    #             cards.append(str(card))
    #     if style == "horizontal":
    #         try:
    #             repr_string = "Foo"
    #             # repr_string = "\n".join(map(lambda arg: "{:<{maxwidth}}".format(arg, maxwidth=self.maxwidth),
    #             #                      ["".join([card.split("\n")[index][:card_width] + " " * max(card_width - 7, 0)
    #             #                                 for card in cards[:-1]])
    #             #                         + cards[-1].split("\n")[index]
    #             #                                         for index in range(7)]))
    #         except IndexError:
    #             # No cards to be displayed
    #             repr_string = "\n".join([" " * self.maxwidth] * 7)
    #     elif style == "vertical":
    #         try:
    #             repr_string = "\n".join([l for c in cards[:-1] for l in str(c).split("\n")[:4]]) + "\n" + cards[-1]
    #         except IndexError:
    #             repr_string = "\n".join([" " * 7] * 7)
    #     elif style == "hidden":
    #         # Print a single box with the player's name and the number of cards in the
    #         # player's hand in it.
    #         top = u"\u256d" + u"\u2500"*5 + u"\u256e\n"
    #         bottom = u"\u2570"+ u"\u2500"*5 + u"\u256f"
            # interior = (u"\u2502" + u"/" * 5 + u"\u2502\n"
            #             + u"\u2502" + "{:/^5}".format(self.name) + u"\u2502\n"
            #             + u"\u2502" + "/" + "{:/>2}".format(len(self)) + "//" + u"\u2502\n"
            #             + (u"\u2502" + "/" * 5 + u"\u2502\n") * 2 )
    #         repr_string = top + interior + bottom
    #     elif style == "top":
    #         repr_string = str(self[-1])
    #     return repr_string
    # def __repr__(self):
    #     return self.repr(self.style)

# class Deck(list):
#     """
#     A Deck(n, s) is a shuffeled list of n copies of the cartesian product of
#     Deck.suites and ranks from s to 10 and "jack", "queen", "king" and "ace".
#     """
#     suites = ["spades", "hearts", "diamonds", "clubs"]
#     def __init__(self, n=1, start=7):
#         ranks = [str(k) for k in range(start, 11)] + ["jack", "queen", "king", "ace"]
#         base_deck = list(map(lambda c: Card(*c), list(product(ranks, Deck.suites)))) * n
#         shuffle(base_deck)
#         super().__init__(base_deck)
print("Hello, world!")

lambda: None
lambda x: x**2
lambda x, **y: x**2 + y

f(f(f(f(f(f(f(f(f(g(h(i(j(k(x))))))))))))))

f"abc"
u"def"
r"raw"
fr"abc"# A comment

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
    
for _ in range(3):
    print("Foo")

#!/usr/bin/python3

from cardgames import Card, Hand, Deck
from itertools import cycle, zip_longest
import time
import os

class Player(object):
    def __init__(self, game, name):
        self.game = game
        self.name = name
        self.cards = Hand()
    def get_card(self):
        self.cards.append(self.game.deck.pop())

class Pointeur(Player):
    def __repr__(self):
        return self.cards.repr(style="vertical")

class Croupier(Player):
    def __init__(self, game, name):
        super().__init__(game, name)
        self.cards = Hand(maxwidth=11)
    def __repr__(self):
        return self.cards.repr(style="horizontal")

class HumanPlayer(Pointeur):
    def __init__(self, game, name):
        super().__init__(game, name)

class AIPlayer(Pointeur):
    def __init__(self, game, name):
        super().__init__(game, name)

class Game(object):
    def __init__(self, players=3):
        self.deck = Deck(n=6, start=2)
        self.croupier = Croupier(self, "")
        self.player_list = [AIPlayer(self, "") for _ in range(players)] + [self.croupier]
        self.players = cycle(self.player_list)
        for _ in range(2):
            for _ in range(players + 1):
                time.sleep(1)
                self.current_player = next(self.players)
                self.current_player.get_card()
                print(self)
    def __repr__(self):
        anchor = "\x1b7\x1b[1;1f" # ANSI escape sequence to start at row 1, columm 1
        width = 13 * (len(self.player_list) - 1) - 5
        top = "\n".join(map(lambda arg: "{:^{width}}".format(arg, width=width), str(self.croupier).splitlines()))
        bottom = "\n".join(map((" " * 5).join, zip_longest(*[str(p).splitlines() for p in self.player_list[:-1]], fillvalue=" " * 7)))
        return anchor + top + "\n" * 5 + bottom + "\x1b8"

if __name__ == "__main__":
    os.system("clear")
    g = Game()
from itertools import product
from random import shuffle

class Card(object):
    """
    A Card is made up of a suite and a rank, and a fancy unicode __repr__ string.
    """
    symbols = {"spades": u"\u2660",
               "hearts": u"\u2665",
               "diamonds": u"\u2666",
               "clubs": u"\u2663"}
    def __init__(self, rank, suite):
        self.rank = rank
        self.suite = suite
    def __repr__(self):
        """
        Paint a card with the rank in the top left and bottom right corner
        and the suite symbol printed in the middle.
        """
        bottom = u"\u2570"+ u"\u2500"*5 + u"\u256f"
        if len(self.rank) < 3:
            rank = self.rank
        else:
            rank = self.rank[0].upper()
        top = u"\u256d" + u"\u2500"*5 + u"\u256e\n"
        interior = (u"\u2502" + "{:<2}".format(rank) + " " * 3 + u"\u2502\n"
                    + u"\u2502" + "     " + u"\u2502\n"
                    + u"\u2502" + "  " + Card.symbols[self.suite] + "  " + u"\u2502\n"
                    + u"\u2502" + "     " + u"\u2502\n"
                    + u"\u2502" + " " * 3 + "{:>2}".format(rank) + u"\u2502\n")
        return top + interior + bottom
    HIDDEN = u"\u256d" + u"\u2500"*5 + u"\u256e\n"  + (u"\u2502" + "/////" + u"\u2502\n") * 5 + u"\u2570"+ u"\u2500"*5 + u"\u256f"

class Hand(list):
    """
    A Hand() object is just a list of Card() objects, which additionally can be
    addressed by an alphabetical index in order for the user to be able to
    select a card with a single key press.
    """
    alphabet = "123456789abcdefghijklmnopqrstuvwxyz"
    def __init__(self, *args, style="horizontal", name="", maxwidth=59, **kwargs):
        self.style = style
        self.name = name
        self.maxwidth = maxwidth
        super().__init__(*args, **kwargs)
    def __call__(self, index):
        position = Hand.alphabet.find(index.lower())
        if position > -1:
            return self[position]
        else:
            raise IndexError
    # def repr(self, style="horizontal", hide=["1"], card_width=8):
    #     cards = []
    #     for index, card in zip(Hand.alphabet, self):
    #         if index in hide:
    #             cards.append(Card.HIDDEN)
    #         else:
    #             cards.append(str(card))
    #     if style == "horizontal":
    #         try:
    #             repr_string = "Foo"
    #             # repr_string = "\n".join(map(lambda arg: "{:<{maxwidth}}".format(arg, maxwidth=self.maxwidth),
    #             #                      ["".join([card.split("\n")[index][:card_width] + " " * max(card_width - 7, 0)
    #             #                                 for card in cards[:-1]])
    #             #                         + cards[-1].split("\n")[index]
    #             #                                         for index in range(7)]))
    #         except IndexError:
    #             # No cards to be displayed
    #             repr_string = "\n".join([" " * self.maxwidth] * 7)
    #     elif style == "vertical":
    #         try:
    #             repr_string = "\n".join([l for c in cards[:-1] for l in str(c).split("\n")[:4]]) + "\n" + cards[-1]
    #         except IndexError:
    #             repr_string = "\n".join([" " * 7] * 7)
    #     elif style == "hidden":
    #         # Print a single box with the player's name and the number of cards in the
    #         # player's hand in it.
    #         top = u"\u256d" + u"\u2500"*5 + u"\u256e\n"
    #         bottom = u"\u2570"+ u"\u2500"*5 + u"\u256f"
            # interior = (u"\u2502" + u"/" * 5 + u"\u2502\n"
            #             + u"\u2502" + "{:/^5}".format(self.name) + u"\u2502\n"
            #             + u"\u2502" + "/" + "{:/>2}".format(len(self)) + "//" + u"\u2502\n"
            #             + (u"\u2502" + "/" * 5 + u"\u2502\n") * 2 )
    #         repr_string = top + interior + bottom
    #     elif style == "top":
    #         repr_string = str(self[-1])
    #     return repr_string
    # def __repr__(self):
    #     return self.repr(self.style)

# class Deck(list):
#     """
#     A Deck(n, s) is a shuffeled list of n copies of the cartesian product of
#     Deck.suites and ranks from s to 10 and "jack", "queen", "king" and "ace".
#     """
#     suites = ["spades", "hearts", "diamonds", "clubs"]
#     def __init__(self, n=1, start=7):
#         ranks = [str(k) for k in range(start, 11)] + ["jack", "queen", "king", "ace"]
#         base_deck = list(map(lambda c: Card(*c), list(product(ranks, Deck.suites)))) * n
#         shuffle(base_deck)
#         super().__init__(base_deck)
print("Hello, world!")

lambda: None
lambda x: x**2
lambda x, **y: x**2 + y

f(f(f(f(f(f(f(f(f(g(h(i(j(k(x))))))))))))))

f"abc"
u"def"
r"raw"
fr"abc"# A comment

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
    
for _ in range(3):
    print("Foo")

#!/usr/bin/python3

from cardgames import Card, Hand, Deck
from itertools import cycle, zip_longest
import time
import os

class Player(object):
    def __init__(self, game, name):
        self.game = game
        self.name = name
        self.cards = Hand()
    def get_card(self):
        self.cards.append(self.game.deck.pop())

class Pointeur(Player):
    def __repr__(self):
        return self.cards.repr(style="vertical")

class Croupier(Player):
    def __init__(self, game, name):
        super().__init__(game, name)
        self.cards = Hand(maxwidth=11)
    def __repr__(self):
        return self.cards.repr(style="horizontal")

class HumanPlayer(Pointeur):
    def __init__(self, game, name):
        super().__init__(game, name)

class AIPlayer(Pointeur):
    def __init__(self, game, name):
        super().__init__(game, name)

class Game(object):
    def __init__(self, players=3):
        self.deck = Deck(n=6, start=2)
        self.croupier = Croupier(self, "")
        self.player_list = [AIPlayer(self, "") for _ in range(players)] + [self.croupier]
        self.players = cycle(self.player_list)
        for _ in range(2):
            for _ in range(players + 1):
                time.sleep(1)
                self.current_player = next(self.players)
                self.current_player.get_card()
                print(self)
    def __repr__(self):
        anchor = "\x1b7\x1b[1;1f" # ANSI escape sequence to start at row 1, columm 1
        width = 13 * (len(self.player_list) - 1) - 5
        top = "\n".join(map(lambda arg: "{:^{width}}".format(arg, width=width), str(self.croupier).splitlines()))
        bottom = "\n".join(map((" " * 5).join, zip_longest(*[str(p).splitlines() for p in self.player_list[:-1]], fillvalue=" " * 7)))
        return anchor + top + "\n" * 5 + bottom + "\x1b8"

if __name__ == "__main__":
    os.system("clear")
    g = Game()
from itertools import product
from random import shuffle

class Card(object):
    """
    A Card is made up of a suite and a rank, and a fancy unicode __repr__ string.
    """
    symbols = {"spades": u"\u2660",
               "hearts": u"\u2665",
               "diamonds": u"\u2666",
               "clubs": u"\u2663"}
    def __init__(self, rank, suite):
        self.rank = rank
        self.suite = suite
    def __repr__(self):
        """
        Paint a card with the rank in the top left and bottom right corner
        and the suite symbol printed in the middle.
        """
        bottom = u"\u2570"+ u"\u2500"*5 + u"\u256f"
        if len(self.rank) < 3:
            rank = self.rank
        else:
            rank = self.rank[0].upper()
        top = u"\u256d" + u"\u2500"*5 + u"\u256e\n"
        interior = (u"\u2502" + "{:<2}".format(rank) + " " * 3 + u"\u2502\n"
                    + u"\u2502" + "     " + u"\u2502\n"
                    + u"\u2502" + "  " + Card.symbols[self.suite] + "  " + u"\u2502\n"
                    + u"\u2502" + "     " + u"\u2502\n"
                    + u"\u2502" + " " * 3 + "{:>2}".format(rank) + u"\u2502\n")
        return top + interior + bottom
    HIDDEN = u"\u256d" + u"\u2500"*5 + u"\u256e\n"  + (u"\u2502" + "/////" + u"\u2502\n") * 5 + u"\u2570"+ u"\u2500"*5 + u"\u256f"

class Hand(list):
    """
    A Hand() object is just a list of Card() objects, which additionally can be
    addressed by an alphabetical index in order for the user to be able to
    select a card with a single key press.
    """
    alphabet = "123456789abcdefghijklmnopqrstuvwxyz"
    def __init__(self, *args, style="horizontal", name="", maxwidth=59, **kwargs):
        self.style = style
        self.name = name
        self.maxwidth = maxwidth
        super().__init__(*args, **kwargs)
    def __call__(self, index):
        position = Hand.alphabet.find(index.lower())
        if position > -1:
            return self[position]
        else:
            raise IndexError
    # def repr(self, style="horizontal", hide=["1"], card_width=8):
    #     cards = []
    #     for index, card in zip(Hand.alphabet, self):
    #         if index in hide:
    #             cards.append(Card.HIDDEN)
    #         else:
    #             cards.append(str(card))
    #     if style == "horizontal":
    #         try:
    #             repr_string = "Foo"
    #             # repr_string = "\n".join(map(lambda arg: "{:<{maxwidth}}".format(arg, maxwidth=self.maxwidth),
    #             #                      ["".join([card.split("\n")[index][:card_width] + " " * max(card_width - 7, 0)
    #             #                                 for card in cards[:-1]])
    #             #                         + cards[-1].split("\n")[index]
    #             #                                         for index in range(7)]))
    #         except IndexError:
    #             # No cards to be displayed
    #             repr_string = "\n".join([" " * self.maxwidth] * 7)
    #     elif style == "vertical":
    #         try:
    #             repr_string = "\n".join([l for c in cards[:-1] for l in str(c).split("\n")[:4]]) + "\n" + cards[-1]
    #         except IndexError:
    #             repr_string = "\n".join([" " * 7] * 7)
    #     elif style == "hidden":
    #         # Print a single box with the player's name and the number of cards in the
    #         # player's hand in it.
    #         top = u"\u256d" + u"\u2500"*5 + u"\u256e\n"
    #         bottom = u"\u2570"+ u"\u2500"*5 + u"\u256f"
            # interior = (u"\u2502" + u"/" * 5 + u"\u2502\n"
            #             + u"\u2502" + "{:/^5}".format(self.name) + u"\u2502\n"
            #             + u"\u2502" + "/" + "{:/>2}".format(len(self)) + "//" + u"\u2502\n"
            #             + (u"\u2502" + "/" * 5 + u"\u2502\n") * 2 )
    #         repr_string = top + interior + bottom
    #     elif style == "top":
    #         repr_string = str(self[-1])
    #     return repr_string
    # def __repr__(self):
    #     return self.repr(self.style)

# class Deck(list):
#     """
#     A Deck(n, s) is a shuffeled list of n copies of the cartesian product of
#     Deck.suites and ranks from s to 10 and "jack", "queen", "king" and "ace".
#     """
#     suites = ["spades", "hearts", "diamonds", "clubs"]
#     def __init__(self, n=1, start=7):
#         ranks = [str(k) for k in range(start, 11)] + ["jack", "queen", "king", "ace"]
#         base_deck = list(map(lambda c: Card(*c), list(product(ranks, Deck.suites)))) * n
#         shuffle(base_deck)
#         super().__init__(base_deck)
print("Hello, world!")

lambda: None
lambda x: x**2
lambda x, **y: x**2 + y

f(f(f(f(f(f(f(f(f(g(h(i(j(k(x))))))))))))))

f"abc"
u"def"
r"raw"
fr"abc"# A comment

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
    
for _ in range(3):
    print("Foo")

#!/usr/bin/python3

from cardgames import Card, Hand, Deck
from itertools import cycle, zip_longest
import time
import os

class Player(object):
    def __init__(self, game, name):
        self.game = game
        self.name = name
        self.cards = Hand()
    def get_card(self):
        self.cards.append(self.game.deck.pop())

class Pointeur(Player):
    def __repr__(self):
        return self.cards.repr(style="vertical")

class Croupier(Player):
    def __init__(self, game, name):
        super().__init__(game, name)
        self.cards = Hand(maxwidth=11)
    def __repr__(self):
        return self.cards.repr(style="horizontal")

class HumanPlayer(Pointeur):
    def __init__(self, game, name):
        super().__init__(game, name)

class AIPlayer(Pointeur):
    def __init__(self, game, name):
        super().__init__(game, name)

class Game(object):
    def __init__(self, players=3):
        self.deck = Deck(n=6, start=2)
        self.croupier = Croupier(self, "")
        self.player_list = [AIPlayer(self, "") for _ in range(players)] + [self.croupier]
        self.players = cycle(self.player_list)
        for _ in range(2):
            for _ in range(players + 1):
                time.sleep(1)
                self.current_player = next(self.players)
                self.current_player.get_card()
                print(self)
    def __repr__(self):
        anchor = "\x1b7\x1b[1;1f" # ANSI escape sequence to start at row 1, columm 1
        width = 13 * (len(self.player_list) - 1) - 5
        top = "\n".join(map(lambda arg: "{:^{width}}".format(arg, width=width), str(self.croupier).splitlines()))
        bottom = "\n".join(map((" " * 5).join, zip_longest(*[str(p).splitlines() for p in self.player_list[:-1]], fillvalue=" " * 7)))
        return anchor + top + "\n" * 5 + bottom + "\x1b8"

if __name__ == "__main__":
    os.system("clear")
    g = Game()
from itertools import product
from random import shuffle

class Card(object):
    """
    A Card is made up of a suite and a rank, and a fancy unicode __repr__ string.
    """
    symbols = {"spades": u"\u2660",
               "hearts": u"\u2665",
               "diamonds": u"\u2666",
               "clubs": u"\u2663"}
    def __init__(self, rank, suite):
        self.rank = rank
        self.suite = suite
    def __repr__(self):
        """
        Paint a card with the rank in the top left and bottom right corner
        and the suite symbol printed in the middle.
        """
        bottom = u"\u2570"+ u"\u2500"*5 + u"\u256f"
        if len(self.rank) < 3:
            rank = self.rank
        else:
            rank = self.rank[0].upper()
        top = u"\u256d" + u"\u2500"*5 + u"\u256e\n"
        interior = (u"\u2502" + "{:<2}".format(rank) + " " * 3 + u"\u2502\n"
                    + u"\u2502" + "     " + u"\u2502\n"
                    + u"\u2502" + "  " + Card.symbols[self.suite] + "  " + u"\u2502\n"
                    + u"\u2502" + "     " + u"\u2502\n"
                    + u"\u2502" + " " * 3 + "{:>2}".format(rank) + u"\u2502\n")
        return top + interior + bottom
    HIDDEN = u"\u256d" + u"\u2500"*5 + u"\u256e\n"  + (u"\u2502" + "/////" + u"\u2502\n") * 5 + u"\u2570"+ u"\u2500"*5 + u"\u256f"

class Hand(list):
    """
    A Hand() object is just a list of Card() objects, which additionally can be
    addressed by an alphabetical index in order for the user to be able to
    select a card with a single key press.
    """
    alphabet = "123456789abcdefghijklmnopqrstuvwxyz"
    def __init__(self, *args, style="horizontal", name="", maxwidth=59, **kwargs):
        self.style = style
        self.name = name
        self.maxwidth = maxwidth
        super().__init__(*args, **kwargs)
    def __call__(self, index):
        position = Hand.alphabet.find(index.lower())
        if position > -1:
            return self[position]
        else:
            raise IndexError
    # def repr(self, style="horizontal", hide=["1"], card_width=8):
    #     cards = []
    #     for index, card in zip(Hand.alphabet, self):
    #         if index in hide:
    #             cards.append(Card.HIDDEN)
    #         else:
    #             cards.append(str(card))
    #     if style == "horizontal":
    #         try:
    #             repr_string = "Foo"
    #             # repr_string = "\n".join(map(lambda arg: "{:<{maxwidth}}".format(arg, maxwidth=self.maxwidth),
    #             #                      ["".join([card.split("\n")[index][:card_width] + " " * max(card_width - 7, 0)
    #             #                                 for card in cards[:-1]])
    #             #                         + cards[-1].split("\n")[index]
    #             #                                         for index in range(7)]))
    #         except IndexError:
    #             # No cards to be displayed
    #             repr_string = "\n".join([" " * self.maxwidth] * 7)
    #     elif style == "vertical":
    #         try:
    #             repr_string = "\n".join([l for c in cards[:-1] for l in str(c).split("\n")[:4]]) + "\n" + cards[-1]
    #         except IndexError:
    #             repr_string = "\n".join([" " * 7] * 7)
    #     elif style == "hidden":
    #         # Print a single box with the player's name and the number of cards in the
    #         # player's hand in it.
    #         top = u"\u256d" + u"\u2500"*5 + u"\u256e\n"
    #         bottom = u"\u2570"+ u"\u2500"*5 + u"\u256f"
            # interior = (u"\u2502" + u"/" * 5 + u"\u2502\n"
            #             + u"\u2502" + "{:/^5}".format(self.name) + u"\u2502\n"
            #             + u"\u2502" + "/" + "{:/>2}".format(len(self)) + "//" + u"\u2502\n"
            #             + (u"\u2502" + "/" * 5 + u"\u2502\n") * 2 )
    #         repr_string = top + interior + bottom
    #     elif style == "top":
    #         repr_string = str(self[-1])
    #     return repr_string
    # def __repr__(self):
    #     return self.repr(self.style)

# class Deck(list):
#     """
#     A Deck(n, s) is a shuffeled list of n copies of the cartesian product of
#     Deck.suites and ranks from s to 10 and "jack", "queen", "king" and "ace".
#     """
#     suites = ["spades", "hearts", "diamonds", "clubs"]
#     def __init__(self, n=1, start=7):
#         ranks = [str(k) for k in range(start, 11)] + ["jack", "queen", "king", "ace"]
#         base_deck = list(map(lambda c: Card(*c), list(product(ranks, Deck.suites)))) * n
#         shuffle(base_deck)
#         super().__init__(base_deck)
print("Hello, world!")

lambda: None
lambda x: x**2
lambda x, **y: x**2 + y

f(f(f(f(f(f(f(f(f(g(h(i(j(k(x))))))))))))))

f"abc"
u"def"
r"raw"
fr"abc"# A comment

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
    
for _ in range(3):
    print("Foo")

#!/usr/bin/python3

from cardgames import Card, Hand, Deck
from itertools import cycle, zip_longest
import time
import os

class Player(object):
    def __init__(self, game, name):
        self.game = game
        self.name = name
        self.cards = Hand()
    def get_card(self):
        self.cards.append(self.game.deck.pop())

class Pointeur(Player):
    def __repr__(self):
        return self.cards.repr(style="vertical")

class Croupier(Player):
    def __init__(self, game, name):
        super().__init__(game, name)
        self.cards = Hand(maxwidth=11)
    def __repr__(self):
        return self.cards.repr(style="horizontal")

class HumanPlayer(Pointeur):
    def __init__(self, game, name):
        super().__init__(game, name)

class AIPlayer(Pointeur):
    def __init__(self, game, name):
        super().__init__(game, name)

class Game(object):
    def __init__(self, players=3):
        self.deck = Deck(n=6, start=2)
        self.croupier = Croupier(self, "")
        self.player_list = [AIPlayer(self, "") for _ in range(players)] + [self.croupier]
        self.players = cycle(self.player_list)
        for _ in range(2):
            for _ in range(players + 1):
                time.sleep(1)
                self.current_player = next(self.players)
                self.current_player.get_card()
                print(self)
    def __repr__(self):
        anchor = "\x1b7\x1b[1;1f" # ANSI escape sequence to start at row 1, columm 1
        width = 13 * (len(self.player_list) - 1) - 5
        top = "\n".join(map(lambda arg: "{:^{width}}".format(arg, width=width), str(self.croupier).splitlines()))
        bottom = "\n".join(map((" " * 5).join, zip_longest(*[str(p).splitlines() for p in self.player_list[:-1]], fillvalue=" " * 7)))
        return anchor + top + "\n" * 5 + bottom + "\x1b8"

if __name__ == "__main__":
    os.system("clear")
    g = Game()
from itertools import product
from random import shuffle

class Card(object):
    """
    A Card is made up of a suite and a rank, and a fancy unicode __repr__ string.
    """
    symbols = {"spades": u"\u2660",
               "hearts": u"\u2665",
               "diamonds": u"\u2666",
               "clubs": u"\u2663"}
    def __init__(self, rank, suite):
        self.rank = rank
        self.suite = suite
    def __repr__(self):
        """
        Paint a card with the rank in the top left and bottom right corner
        and the suite symbol printed in the middle.
        """
        bottom = u"\u2570"+ u"\u2500"*5 + u"\u256f"
        if len(self.rank) < 3:
            rank = self.rank
        else:
            rank = self.rank[0].upper()
        top = u"\u256d" + u"\u2500"*5 + u"\u256e\n"
        interior = (u"\u2502" + "{:<2}".format(rank) + " " * 3 + u"\u2502\n"
                    + u"\u2502" + "     " + u"\u2502\n"
                    + u"\u2502" + "  " + Card.symbols[self.suite] + "  " + u"\u2502\n"
                    + u"\u2502" + "     " + u"\u2502\n"
                    + u"\u2502" + " " * 3 + "{:>2}".format(rank) + u"\u2502\n")
        return top + interior + bottom
    HIDDEN = u"\u256d" + u"\u2500"*5 + u"\u256e\n"  + (u"\u2502" + "/////" + u"\u2502\n") * 5 + u"\u2570"+ u"\u2500"*5 + u"\u256f"

class Hand(list):
    """
    A Hand() object is just a list of Card() objects, which additionally can be
    addressed by an alphabetical index in order for the user to be able to
    select a card with a single key press.
    """
    alphabet = "123456789abcdefghijklmnopqrstuvwxyz"
    def __init__(self, *args, style="horizontal", name="", maxwidth=59, **kwargs):
        self.style = style
        self.name = name
        self.maxwidth = maxwidth
        super().__init__(*args, **kwargs)
    def __call__(self, index):
        position = Hand.alphabet.find(index.lower())
        if position > -1:
            return self[position]
        else:
            raise IndexError
    # def repr(self, style="horizontal", hide=["1"], card_width=8):
    #     cards = []
    #     for index, card in zip(Hand.alphabet, self):
    #         if index in hide:
    #             cards.append(Card.HIDDEN)
    #         else:
    #             cards.append(str(card))
    #     if style == "horizontal":
    #         try:
    #             repr_string = "Foo"
    #             # repr_string = "\n".join(map(lambda arg: "{:<{maxwidth}}".format(arg, maxwidth=self.maxwidth),
    #             #                      ["".join([card.split("\n")[index][:card_width] + " " * max(card_width - 7, 0)
    #             #                                 for card in cards[:-1]])
    #             #                         + cards[-1].split("\n")[index]
    #             #                                         for index in range(7)]))
    #         except IndexError:
    #             # No cards to be displayed
    #             repr_string = "\n".join([" " * self.maxwidth] * 7)
    #     elif style == "vertical":
    #         try:
    #             repr_string = "\n".join([l for c in cards[:-1] for l in str(c).split("\n")[:4]]) + "\n" + cards[-1]
    #         except IndexError:
    #             repr_string = "\n".join([" " * 7] * 7)
    #     elif style == "hidden":
    #         # Print a single box with the player's name and the number of cards in the
    #         # player's hand in it.
    #         top = u"\u256d" + u"\u2500"*5 + u"\u256e\n"
    #         bottom = u"\u2570"+ u"\u2500"*5 + u"\u256f"
            # interior = (u"\u2502" + u"/" * 5 + u"\u2502\n"
            #             + u"\u2502" + "{:/^5}".format(self.name) + u"\u2502\n"
            #             + u"\u2502" + "/" + "{:/>2}".format(len(self)) + "//" + u"\u2502\n"
            #             + (u"\u2502" + "/" * 5 + u"\u2502\n") * 2 )
    #         repr_string = top + interior + bottom
    #     elif style == "top":
    #         repr_string = str(self[-1])
    #     return repr_string
    # def __repr__(self):
    #     return self.repr(self.style)

# class Deck(list):
#     """
#     A Deck(n, s) is a shuffeled list of n copies of the cartesian product of
#     Deck.suites and ranks from s to 10 and "jack", "queen", "king" and "ace".
#     """
#     suites = ["spades", "hearts", "diamonds", "clubs"]
#     def __init__(self, n=1, start=7):
#         ranks = [str(k) for k in range(start, 11)] + ["jack", "queen", "king", "ace"]
#         base_deck = list(map(lambda c: Card(*c), list(product(ranks, Deck.suites)))) * n
#         shuffle(base_deck)
#         super().__init__(base_deck)
print("Hello, world!")

lambda: None
lambda x: x**2
lambda x, **y: x**2 + y

f(f(f(f(f(f(f(f(f(g(h(i(j(k(x))))))))))))))

f"abc"
u"def"
r"raw"
fr"abc"# A comment

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
    
for _ in range(3):
    print("Foo")

#!/usr/bin/python3

from cardgames import Card, Hand, Deck
from itertools import cycle, zip_longest
import time
import os

class Player(object):
    def __init__(self, game, name):
        self.game = game
        self.name = name
        self.cards = Hand()
    def get_card(self):
        self.cards.append(self.game.deck.pop())

class Pointeur(Player):
    def __repr__(self):
        return self.cards.repr(style="vertical")

class Croupier(Player):
    def __init__(self, game, name):
        super().__init__(game, name)
        self.cards = Hand(maxwidth=11)
    def __repr__(self):
        return self.cards.repr(style="horizontal")

class HumanPlayer(Pointeur):
    def __init__(self, game, name):
        super().__init__(game, name)

class AIPlayer(Pointeur):
    def __init__(self, game, name):
        super().__init__(game, name)

class Game(object):
    def __init__(self, players=3):
        self.deck = Deck(n=6, start=2)
        self.croupier = Croupier(self, "")
        self.player_list = [AIPlayer(self, "") for _ in range(players)] + [self.croupier]
        self.players = cycle(self.player_list)
        for _ in range(2):
            for _ in range(players + 1):
                time.sleep(1)
                self.current_player = next(self.players)
                self.current_player.get_card()
                print(self)
    def __repr__(self):
        anchor = "\x1b7\x1b[1;1f" # ANSI escape sequence to start at row 1, columm 1
        width = 13 * (len(self.player_list) - 1) - 5
        top = "\n".join(map(lambda arg: "{:^{width}}".format(arg, width=width), str(self.croupier).splitlines()))
        bottom = "\n".join(map((" " * 5).join, zip_longest(*[str(p).splitlines() for p in self.player_list[:-1]], fillvalue=" " * 7)))
        return anchor + top + "\n" * 5 + bottom + "\x1b8"

if __name__ == "__main__":
    os.system("clear")
    g = Game()
from itertools import product
from random import shuffle

class Card(object):
    """
    A Card is made up of a suite and a rank, and a fancy unicode __repr__ string.
    """
    symbols = {"spades": u"\u2660",
               "hearts": u"\u2665",
               "diamonds": u"\u2666",
               "clubs": u"\u2663"}
    def __init__(self, rank, suite):
        self.rank = rank
        self.suite = suite
    def __repr__(self):
        """
        Paint a card with the rank in the top left and bottom right corner
        and the suite symbol printed in the middle.
        """
        bottom = u"\u2570"+ u"\u2500"*5 + u"\u256f"
        if len(self.rank) < 3:
            rank = self.rank
        else:
            rank = self.rank[0].upper()
        top = u"\u256d" + u"\u2500"*5 + u"\u256e\n"
        interior = (u"\u2502" + "{:<2}".format(rank) + " " * 3 + u"\u2502\n"
                    + u"\u2502" + "     " + u"\u2502\n"
                    + u"\u2502" + "  " + Card.symbols[self.suite] + "  " + u"\u2502\n"
                    + u"\u2502" + "     " + u"\u2502\n"
                    + u"\u2502" + " " * 3 + "{:>2}".format(rank) + u"\u2502\n")
        return top + interior + bottom
    HIDDEN = u"\u256d" + u"\u2500"*5 + u"\u256e\n"  + (u"\u2502" + "/////" + u"\u2502\n") * 5 + u"\u2570"+ u"\u2500"*5 + u"\u256f"

class Hand(list):
    """
    A Hand() object is just a list of Card() objects, which additionally can be
    addressed by an alphabetical index in order for the user to be able to
    select a card with a single key press.
    """
    alphabet = "123456789abcdefghijklmnopqrstuvwxyz"
    def __init__(self, *args, style="horizontal", name="", maxwidth=59, **kwargs):
        self.style = style
        self.name = name
        self.maxwidth = maxwidth
        super().__init__(*args, **kwargs)
    def __call__(self, index):
        position = Hand.alphabet.find(index.lower())
        if position > -1:
            return self[position]
        else:
            raise IndexError
    # def repr(self, style="horizontal", hide=["1"], card_width=8):
    #     cards = []
    #     for index, card in zip(Hand.alphabet, self):
    #         if index in hide:
    #             cards.append(Card.HIDDEN)
    #         else:
    #             cards.append(str(card))
    #     if style == "horizontal":
    #         try:
    #             repr_string = "Foo"
    #             # repr_string = "\n".join(map(lambda arg: "{:<{maxwidth}}".format(arg, maxwidth=self.maxwidth),
    #             #                      ["".join([card.split("\n")[index][:card_width] + " " * max(card_width - 7, 0)
    #             #                                 for card in cards[:-1]])
    #             #                         + cards[-1].split("\n")[index]
    #             #                                         for index in range(7)]))
    #         except IndexError:
    #             # No cards to be displayed
    #             repr_string = "\n".join([" " * self.maxwidth] * 7)
    #     elif style == "vertical":
    #         try:
    #             repr_string = "\n".join([l for c in cards[:-1] for l in str(c).split("\n")[:4]]) + "\n" + cards[-1]
    #         except IndexError:
    #             repr_string = "\n".join([" " * 7] * 7)
    #     elif style == "hidden":
    #         # Print a single box with the player's name and the number of cards in the
    #         # player's hand in it.
    #         top = u"\u256d" + u"\u2500"*5 + u"\u256e\n"
    #         bottom = u"\u2570"+ u"\u2500"*5 + u"\u256f"
            # interior = (u"\u2502" + u"/" * 5 + u"\u2502\n"
            #             + u"\u2502" + "{:/^5}".format(self.name) + u"\u2502\n"
            #             + u"\u2502" + "/" + "{:/>2}".format(len(self)) + "//" + u"\u2502\n"
            #             + (u"\u2502" + "/" * 5 + u"\u2502\n") * 2 )
    #         repr_string = top + interior + bottom
    #     elif style == "top":
    #         repr_string = str(self[-1])
    #     return repr_string
    # def __repr__(self):
    #     return self.repr(self.style)

# class Deck(list):
#     """
#     A Deck(n, s) is a shuffeled list of n copies of the cartesian product of
#     Deck.suites and ranks from s to 10 and "jack", "queen", "king" and "ace".
#     """
#     suites = ["spades", "hearts", "diamonds", "clubs"]
#     def __init__(self, n=1, start=7):
#         ranks = [str(k) for k in range(start, 11)] + ["jack", "queen", "king", "ace"]
#         base_deck = list(map(lambda c: Card(*c), list(product(ranks, Deck.suites)))) * n
#         shuffle(base_deck)
#         super().__init__(base_deck)
print("Hello, world!")

lambda: None
lambda x: x**2
lambda x, **y: x**2 + y

f(f(f(f(f(f(f(f(f(g(h(i(j(k(x))))))))))))))

f"abc"
u"def"
r"raw"
fr"abc"# A comment

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
    
for _ in range(3):
    print("Foo")

#!/usr/bin/python3

from cardgames import Card, Hand, Deck
from itertools import cycle, zip_longest
import time
import os

class Player(object):
    def __init__(self, game, name):
        self.game = game
        self.name = name
        self.cards = Hand()
    def get_card(self):
        self.cards.append(self.game.deck.pop())

class Pointeur(Player):
    def __repr__(self):
        return self.cards.repr(style="vertical")

class Croupier(Player):
    def __init__(self, game, name):
        super().__init__(game, name)
        self.cards = Hand(maxwidth=11)
    def __repr__(self):
        return self.cards.repr(style="horizontal")

class HumanPlayer(Pointeur):
    def __init__(self, game, name):
        super().__init__(game, name)

class AIPlayer(Pointeur):
    def __init__(self, game, name):
        super().__init__(game, name)

class Game(object):
    def __init__(self, players=3):
        self.deck = Deck(n=6, start=2)
        self.croupier = Croupier(self, "")
        self.player_list = [AIPlayer(self, "") for _ in range(players)] + [self.croupier]
        self.players = cycle(self.player_list)
        for _ in range(2):
            for _ in range(players + 1):
                time.sleep(1)
                self.current_player = next(self.players)
                self.current_player.get_card()
                print(self)
    def __repr__(self):
        anchor = "\x1b7\x1b[1;1f" # ANSI escape sequence to start at row 1, columm 1
        width = 13 * (len(self.player_list) - 1) - 5
        top = "\n".join(map(lambda arg: "{:^{width}}".format(arg, width=width), str(self.croupier).splitlines()))
        bottom = "\n".join(map((" " * 5).join, zip_longest(*[str(p).splitlines() for p in self.player_list[:-1]], fillvalue=" " * 7)))
        return anchor + top + "\n" * 5 + bottom + "\x1b8"

if __name__ == "__main__":
    os.system("clear")
    g = Game()
from itertools import product
from random import shuffle

class Card(object):
    """
    A Card is made up of a suite and a rank, and a fancy unicode __repr__ string.
    """
    symbols = {"spades": u"\u2660",
               "hearts": u"\u2665",
               "diamonds": u"\u2666",
               "clubs": u"\u2663"}
    def __init__(self, rank, suite):
        self.rank = rank
        self.suite = suite
    def __repr__(self):
        """
        Paint a card with the rank in the top left and bottom right corner
        and the suite symbol printed in the middle.
        """
        bottom = u"\u2570"+ u"\u2500"*5 + u"\u256f"
        if len(self.rank) < 3:
            rank = self.rank
        else:
            rank = self.rank[0].upper()
        top = u"\u256d" + u"\u2500"*5 + u"\u256e\n"
        interior = (u"\u2502" + "{:<2}".format(rank) + " " * 3 + u"\u2502\n"
                    + u"\u2502" + "     " + u"\u2502\n"
                    + u"\u2502" + "  " + Card.symbols[self.suite] + "  " + u"\u2502\n"
                    + u"\u2502" + "     " + u"\u2502\n"
                    + u"\u2502" + " " * 3 + "{:>2}".format(rank) + u"\u2502\n")
        return top + interior + bottom
    HIDDEN = u"\u256d" + u"\u2500"*5 + u"\u256e\n"  + (u"\u2502" + "/////" + u"\u2502\n") * 5 + u"\u2570"+ u"\u2500"*5 + u"\u256f"

class Hand(list):
    """
    A Hand() object is just a list of Card() objects, which additionally can be
    addressed by an alphabetical index in order for the user to be able to
    select a card with a single key press.
    """
    alphabet = "123456789abcdefghijklmnopqrstuvwxyz"
    def __init__(self, *args, style="horizontal", name="", maxwidth=59, **kwargs):
        self.style = style
        self.name = name
        self.maxwidth = maxwidth
        super().__init__(*args, **kwargs)
    def __call__(self, index):
        position = Hand.alphabet.find(index.lower())
        if position > -1:
            return self[position]
        else:
            raise IndexError
    # def repr(self, style="horizontal", hide=["1"], card_width=8):
    #     cards = []
    #     for index, card in zip(Hand.alphabet, self):
    #         if index in hide:
    #             cards.append(Card.HIDDEN)
    #         else:
    #             cards.append(str(card))
    #     if style == "horizontal":
    #         try:
    #             repr_string = "Foo"
    #             # repr_string = "\n".join(map(lambda arg: "{:<{maxwidth}}".format(arg, maxwidth=self.maxwidth),
    #             #                      ["".join([card.split("\n")[index][:card_width] + " " * max(card_width - 7, 0)
    #             #                                 for card in cards[:-1]])
    #             #                         + cards[-1].split("\n")[index]
    #             #                                         for index in range(7)]))
    #         except IndexError:
    #             # No cards to be displayed
    #             repr_string = "\n".join([" " * self.maxwidth] * 7)
    #     elif style == "vertical":
    #         try:
    #             repr_string = "\n".join([l for c in cards[:-1] for l in str(c).split("\n")[:4]]) + "\n" + cards[-1]
    #         except IndexError:
    #             repr_string = "\n".join([" " * 7] * 7)
    #     elif style == "hidden":
    #         # Print a single box with the player's name and the number of cards in the
    #         # player's hand in it.
    #         top = u"\u256d" + u"\u2500"*5 + u"\u256e\n"
    #         bottom = u"\u2570"+ u"\u2500"*5 + u"\u256f"
            # interior = (u"\u2502" + u"/" * 5 + u"\u2502\n"
            #             + u"\u2502" + "{:/^5}".format(self.name) + u"\u2502\n"
            #             + u"\u2502" + "/" + "{:/>2}".format(len(self)) + "//" + u"\u2502\n"
            #             + (u"\u2502" + "/" * 5 + u"\u2502\n") * 2 )
    #         repr_string = top + interior + bottom
    #     elif style == "top":
    #         repr_string = str(self[-1])
    #     return repr_string
    # def __repr__(self):
    #     return self.repr(self.style)

# class Deck(list):
#     """
#     A Deck(n, s) is a shuffeled list of n copies of the cartesian product of
#     Deck.suites and ranks from s to 10 and "jack", "queen", "king" and "ace".
#     """
#     suites = ["spades", "hearts", "diamonds", "clubs"]
#     def __init__(self, n=1, start=7):
#         ranks = [str(k) for k in range(start, 11)] + ["jack", "queen", "king", "ace"]
#         base_deck = list(map(lambda c: Card(*c), list(product(ranks, Deck.suites)))) * n
#         shuffle(base_deck)
#         super().__init__(base_deck)
print("Hello, world!")

lambda: None
lambda x: x**2
lambda x, **y: x**2 + y

f(f(f(f(f(f(f(f(f(g(h(i(j(k(x))))))))))))))

f"abc"
u"def"
r"raw"
fr"abc"# A comment

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
    
for _ in range(3):
    print("Foo")

#!/usr/bin/python3

from cardgames import Card, Hand, Deck
from itertools import cycle, zip_longest
import time
import os

class Player(object):
    def __init__(self, game, name):
        self.game = game
        self.name = name
        self.cards = Hand()
    def get_card(self):
        self.cards.append(self.game.deck.pop())

class Pointeur(Player):
    def __repr__(self):
        return self.cards.repr(style="vertical")

class Croupier(Player):
    def __init__(self, game, name):
        super().__init__(game, name)
        self.cards = Hand(maxwidth=11)
    def __repr__(self):
        return self.cards.repr(style="horizontal")

class HumanPlayer(Pointeur):
    def __init__(self, game, name):
        super().__init__(game, name)

class AIPlayer(Pointeur):
    def __init__(self, game, name):
        super().__init__(game, name)

class Game(object):
    def __init__(self, players=3):
        self.deck = Deck(n=6, start=2)
        self.croupier = Croupier(self, "")
        self.player_list = [AIPlayer(self, "") for _ in range(players)] + [self.croupier]
        self.players = cycle(self.player_list)
        for _ in range(2):
            for _ in range(players + 1):
                time.sleep(1)
                self.current_player = next(self.players)
                self.current_player.get_card()
                print(self)
    def __repr__(self):
        anchor = "\x1b7\x1b[1;1f" # ANSI escape sequence to start at row 1, columm 1
        width = 13 * (len(self.player_list) - 1) - 5
        top = "\n".join(map(lambda arg: "{:^{width}}".format(arg, width=width), str(self.croupier).splitlines()))
        bottom = "\n".join(map((" " * 5).join, zip_longest(*[str(p).splitlines() for p in self.player_list[:-1]], fillvalue=" " * 7)))
        return anchor + top + "\n" * 5 + bottom + "\x1b8"

if __name__ == "__main__":
    os.system("clear")
    g = Game()
from itertools import product
from random import shuffle

class Card(object):
    """
    A Card is made up of a suite and a rank, and a fancy unicode __repr__ string.
    """
    symbols = {"spades": u"\u2660",
               "hearts": u"\u2665",
               "diamonds": u"\u2666",
               "clubs": u"\u2663"}
    def __init__(self, rank, suite):
        self.rank = rank
        self.suite = suite
    def __repr__(self):
        """
        Paint a card with the rank in the top left and bottom right corner
        and the suite symbol printed in the middle.
        """
        bottom = u"\u2570"+ u"\u2500"*5 + u"\u256f"
        if len(self.rank) < 3:
            rank = self.rank
        else:
            rank = self.rank[0].upper()
        top = u"\u256d" + u"\u2500"*5 + u"\u256e\n"
        interior = (u"\u2502" + "{:<2}".format(rank) + " " * 3 + u"\u2502\n"
                    + u"\u2502" + "     " + u"\u2502\n"
                    + u"\u2502" + "  " + Card.symbols[self.suite] + "  " + u"\u2502\n"
                    + u"\u2502" + "     " + u"\u2502\n"
                    + u"\u2502" + " " * 3 + "{:>2}".format(rank) + u"\u2502\n")
        return top + interior + bottom
    HIDDEN = u"\u256d" + u"\u2500"*5 + u"\u256e\n"  + (u"\u2502" + "/////" + u"\u2502\n") * 5 + u"\u2570"+ u"\u2500"*5 + u"\u256f"

class Hand(list):
    """
    A Hand() object is just a list of Card() objects, which additionally can be
    addressed by an alphabetical index in order for the user to be able to
    select a card with a single key press.
    """
    alphabet = "123456789abcdefghijklmnopqrstuvwxyz"
    def __init__(self, *args, style="horizontal", name="", maxwidth=59, **kwargs):
        self.style = style
        self.name = name
        self.maxwidth = maxwidth
        super().__init__(*args, **kwargs)
    def __call__(self, index):
        position = Hand.alphabet.find(index.lower())
        if position > -1:
            return self[position]
        else:
            raise IndexError
    # def repr(self, style="horizontal", hide=["1"], card_width=8):
    #     cards = []
    #     for index, card in zip(Hand.alphabet, self):
    #         if index in hide:
    #             cards.append(Card.HIDDEN)
    #         else:
    #             cards.append(str(card))
    #     if style == "horizontal":
    #         try:
    #             repr_string = "Foo"
    #             # repr_string = "\n".join(map(lambda arg: "{:<{maxwidth}}".format(arg, maxwidth=self.maxwidth),
    #             #                      ["".join([card.split("\n")[index][:card_width] + " " * max(card_width - 7, 0)
    #             #                                 for card in cards[:-1]])
    #             #                         + cards[-1].split("\n")[index]
    #             #                                         for index in range(7)]))
    #         except IndexError:
    #             # No cards to be displayed
    #             repr_string = "\n".join([" " * self.maxwidth] * 7)
    #     elif style == "vertical":
    #         try:
    #             repr_string = "\n".join([l for c in cards[:-1] for l in str(c).split("\n")[:4]]) + "\n" + cards[-1]
    #         except IndexError:
    #             repr_string = "\n".join([" " * 7] * 7)
    #     elif style == "hidden":
    #         # Print a single box with the player's name and the number of cards in the
    #         # player's hand in it.
    #         top = u"\u256d" + u"\u2500"*5 + u"\u256e\n"
    #         bottom = u"\u2570"+ u"\u2500"*5 + u"\u256f"
            # interior = (u"\u2502" + u"/" * 5 + u"\u2502\n"
            #             + u"\u2502" + "{:/^5}".format(self.name) + u"\u2502\n"
            #             + u"\u2502" + "/" + "{:/>2}".format(len(self)) + "//" + u"\u2502\n"
            #             + (u"\u2502" + "/" * 5 + u"\u2502\n") * 2 )
    #         repr_string = top + interior + bottom
    #     elif style == "top":
    #         repr_string = str(self[-1])
    #     return repr_string
    # def __repr__(self):
    #     return self.repr(self.style)

# class Deck(list):
#     """
#     A Deck(n, s) is a shuffeled list of n copies of the cartesian product of
#     Deck.suites and ranks from s to 10 and "jack", "queen", "king" and "ace".
#     """
#     suites = ["spades", "hearts", "diamonds", "clubs"]
#     def __init__(self, n=1, start=7):
#         ranks = [str(k) for k in range(start, 11)] + ["jack", "queen", "king", "ace"]
#         base_deck = list(map(lambda c: Card(*c), list(product(ranks, Deck.suites)))) * n
#         shuffle(base_deck)
#         super().__init__(base_deck)
print("Hello, world!")

lambda: None
lambda x: x**2
lambda x, **y: x**2 + y

f(f(f(f(f(f(f(f(f(g(h(i(j(k(x))))))))))))))

f"abc"
u"def"
r"raw"
fr"abc"# A comment

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
    
for _ in range(3):
    print("Foo")

#!/usr/bin/python3

from cardgames import Card, Hand, Deck
from itertools import cycle, zip_longest
import time
import os

class Player(object):
    def __init__(self, game, name):
        self.game = game
        self.name = name
        self.cards = Hand()
    def get_card(self):
        self.cards.append(self.game.deck.pop())

class Pointeur(Player):
    def __repr__(self):
        return self.cards.repr(style="vertical")

class Croupier(Player):
    def __init__(self, game, name):
        super().__init__(game, name)
        self.cards = Hand(maxwidth=11)
    def __repr__(self):
        return self.cards.repr(style="horizontal")

class HumanPlayer(Pointeur):
    def __init__(self, game, name):
        super().__init__(game, name)

class AIPlayer(Pointeur):
    def __init__(self, game, name):
        super().__init__(game, name)

class Game(object):
    def __init__(self, players=3):
        self.deck = Deck(n=6, start=2)
        self.croupier = Croupier(self, "")
        self.player_list = [AIPlayer(self, "") for _ in range(players)] + [self.croupier]
        self.players = cycle(self.player_list)
        for _ in range(2):
            for _ in range(players + 1):
                time.sleep(1)
                self.current_player = next(self.players)
                self.current_player.get_card()
                print(self)
    def __repr__(self):
        anchor = "\x1b7\x1b[1;1f" # ANSI escape sequence to start at row 1, columm 1
        width = 13 * (len(self.player_list) - 1) - 5
        top = "\n".join(map(lambda arg: "{:^{width}}".format(arg, width=width), str(self.croupier).splitlines()))
        bottom = "\n".join(map((" " * 5).join, zip_longest(*[str(p).splitlines() for p in self.player_list[:-1]], fillvalue=" " * 7)))
        return anchor + top + "\n" * 5 + bottom + "\x1b8"

if __name__ == "__main__":
    os.system("clear")
    g = Game()
from itertools import product
from random import shuffle

class Card(object):
    """
    A Card is made up of a suite and a rank, and a fancy unicode __repr__ string.
    """
    symbols = {"spades": u"\u2660",
               "hearts": u"\u2665",
               "diamonds": u"\u2666",
               "clubs": u"\u2663"}
    def __init__(self, rank, suite):
        self.rank = rank
        self.suite = suite
    def __repr__(self):
        """
        Paint a card with the rank in the top left and bottom right corner
        and the suite symbol printed in the middle.
        """
        bottom = u"\u2570"+ u"\u2500"*5 + u"\u256f"
        if len(self.rank) < 3:
            rank = self.rank
        else:
            rank = self.rank[0].upper()
        top = u"\u256d" + u"\u2500"*5 + u"\u256e\n"
        interior = (u"\u2502" + "{:<2}".format(rank) + " " * 3 + u"\u2502\n"
                    + u"\u2502" + "     " + u"\u2502\n"
                    + u"\u2502" + "  " + Card.symbols[self.suite] + "  " + u"\u2502\n"
                    + u"\u2502" + "     " + u"\u2502\n"
                    + u"\u2502" + " " * 3 + "{:>2}".format(rank) + u"\u2502\n")
        return top + interior + bottom
    HIDDEN = u"\u256d" + u"\u2500"*5 + u"\u256e\n"  + (u"\u2502" + "/////" + u"\u2502\n") * 5 + u"\u2570"+ u"\u2500"*5 + u"\u256f"

class Hand(list):
    """
    A Hand() object is just a list of Card() objects, which additionally can be
    addressed by an alphabetical index in order for the user to be able to
    select a card with a single key press.
    """
    alphabet = "123456789abcdefghijklmnopqrstuvwxyz"
    def __init__(self, *args, style="horizontal", name="", maxwidth=59, **kwargs):
        self.style = style
        self.name = name
        self.maxwidth = maxwidth
        super().__init__(*args, **kwargs)
    def __call__(self, index):
        position = Hand.alphabet.find(index.lower())
        if position > -1:
            return self[position]
        else:
            raise IndexError
    # def repr(self, style="horizontal", hide=["1"], card_width=8):
    #     cards = []
    #     for index, card in zip(Hand.alphabet, self):
    #         if index in hide:
    #             cards.append(Card.HIDDEN)
    #         else:
    #             cards.append(str(card))
    #     if style == "horizontal":
    #         try:
    #             repr_string = "Foo"
    #             # repr_string = "\n".join(map(lambda arg: "{:<{maxwidth}}".format(arg, maxwidth=self.maxwidth),
    #             #                      ["".join([card.split("\n")[index][:card_width] + " " * max(card_width - 7, 0)
    #             #                                 for card in cards[:-1]])
    #             #                         + cards[-1].split("\n")[index]
    #             #                                         for index in range(7)]))
    #         except IndexError:
    #             # No cards to be displayed
    #             repr_string = "\n".join([" " * self.maxwidth] * 7)
    #     elif style == "vertical":
    #         try:
    #             repr_string = "\n".join([l for c in cards[:-1] for l in str(c).split("\n")[:4]]) + "\n" + cards[-1]
    #         except IndexError:
    #             repr_string = "\n".join([" " * 7] * 7)
    #     elif style == "hidden":
    #         # Print a single box with the player's name and the number of cards in the
    #         # player's hand in it.
    #         top = u"\u256d" + u"\u2500"*5 + u"\u256e\n"
    #         bottom = u"\u2570"+ u"\u2500"*5 + u"\u256f"
            # interior = (u"\u2502" + u"/" * 5 + u"\u2502\n"
            #             + u"\u2502" + "{:/^5}".format(self.name) + u"\u2502\n"
            #             + u"\u2502" + "/" + "{:/>2}".format(len(self)) + "//" + u"\u2502\n"
            #             + (u"\u2502" + "/" * 5 + u"\u2502\n") * 2 )
    #         repr_string = top + interior + bottom
    #     elif style == "top":
    #         repr_string = str(self[-1])
    #     return repr_string
    # def __repr__(self):
    #     return self.repr(self.style)

# class Deck(list):
#     """
#     A Deck(n, s) is a shuffeled list of n copies of the cartesian product of
#     Deck.suites and ranks from s to 10 and "jack", "queen", "king" and "ace".
#     """
#     suites = ["spades", "hearts", "diamonds", "clubs"]
#     def __init__(self, n=1, start=7):
#         ranks = [str(k) for k in range(start, 11)] + ["jack", "queen", "king", "ace"]
#         base_deck = list(map(lambda c: Card(*c), list(product(ranks, Deck.suites)))) * n
#         shuffle(base_deck)
#         super().__init__(base_deck)
print("Hello, world!")

lambda: None
lambda x: x**2
lambda x, **y: x**2 + y

f(f(f(f(f(f(f(f(f(g(h(i(j(k(x))))))))))))))

f"abc"
u"def"
r"raw"
fr"abc"# A comment

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
    
for _ in range(3):
    print("Foo")

#!/usr/bin/python3

from cardgames import Card, Hand, Deck
from itertools import cycle, zip_longest
import time
import os

class Player(object):
    def __init__(self, game, name):
        self.game = game
        self.name = name
        self.cards = Hand()
    def get_card(self):
        self.cards.append(self.game.deck.pop())

class Pointeur(Player):
    def __repr__(self):
        return self.cards.repr(style="vertical")

class Croupier(Player):
    def __init__(self, game, name):
        super().__init__(game, name)
        self.cards = Hand(maxwidth=11)
    def __repr__(self):
        return self.cards.repr(style="horizontal")

class HumanPlayer(Pointeur):
    def __init__(self, game, name):
        super().__init__(game, name)

class AIPlayer(Pointeur):
    def __init__(self, game, name):
        super().__init__(game, name)

class Game(object):
    def __init__(self, players=3):
        self.deck = Deck(n=6, start=2)
        self.croupier = Croupier(self, "")
        self.player_list = [AIPlayer(self, "") for _ in range(players)] + [self.croupier]
        self.players = cycle(self.player_list)
        for _ in range(2):
            for _ in range(players + 1):
                time.sleep(1)
                self.current_player = next(self.players)
                self.current_player.get_card()
                print(self)
    def __repr__(self):
        anchor = "\x1b7\x1b[1;1f" # ANSI escape sequence to start at row 1, columm 1
        width = 13 * (len(self.player_list) - 1) - 5
        top = "\n".join(map(lambda arg: "{:^{width}}".format(arg, width=width), str(self.croupier).splitlines()))
        bottom = "\n".join(map((" " * 5).join, zip_longest(*[str(p).splitlines() for p in self.player_list[:-1]], fillvalue=" " * 7)))
        return anchor + top + "\n" * 5 + bottom + "\x1b8"

if __name__ == "__main__":
    os.system("clear")
    g = Game()
from itertools import product
from random import shuffle

class Card(object):
    """
    A Card is made up of a suite and a rank, and a fancy unicode __repr__ string.
    """
    symbols = {"spades": u"\u2660",
               "hearts": u"\u2665",
               "diamonds": u"\u2666",
               "clubs": u"\u2663"}
    def __init__(self, rank, suite):
        self.rank = rank
        self.suite = suite
    def __repr__(self):
        """
        Paint a card with the rank in the top left and bottom right corner
        and the suite symbol printed in the middle.
        """
        bottom = u"\u2570"+ u"\u2500"*5 + u"\u256f"
        if len(self.rank) < 3:
            rank = self.rank
        else:
            rank = self.rank[0].upper()
        top = u"\u256d" + u"\u2500"*5 + u"\u256e\n"
        interior = (u"\u2502" + "{:<2}".format(rank) + " " * 3 + u"\u2502\n"
                    + u"\u2502" + "     " + u"\u2502\n"
                    + u"\u2502" + "  " + Card.symbols[self.suite] + "  " + u"\u2502\n"
                    + u"\u2502" + "     " + u"\u2502\n"
                    + u"\u2502" + " " * 3 + "{:>2}".format(rank) + u"\u2502\n")
        return top + interior + bottom
    HIDDEN = u"\u256d" + u"\u2500"*5 + u"\u256e\n"  + (u"\u2502" + "/////" + u"\u2502\n") * 5 + u"\u2570"+ u"\u2500"*5 + u"\u256f"

class Hand(list):
    """
    A Hand() object is just a list of Card() objects, which additionally can be
    addressed by an alphabetical index in order for the user to be able to
    select a card with a single key press.
    """
    alphabet = "123456789abcdefghijklmnopqrstuvwxyz"
    def __init__(self, *args, style="horizontal", name="", maxwidth=59, **kwargs):
        self.style = style
        self.name = name
        self.maxwidth = maxwidth
        super().__init__(*args, **kwargs)
    def __call__(self, index):
        position = Hand.alphabet.find(index.lower())
        if position > -1:
            return self[position]
        else:
            raise IndexError
    # def repr(self, style="horizontal", hide=["1"], card_width=8):
    #     cards = []
    #     for index, card in zip(Hand.alphabet, self):
    #         if index in hide:
    #             cards.append(Card.HIDDEN)
    #         else:
    #             cards.append(str(card))
    #     if style == "horizontal":
    #         try:
    #             repr_string = "Foo"
    #             # repr_string = "\n".join(map(lambda arg: "{:<{maxwidth}}".format(arg, maxwidth=self.maxwidth),
    #             #                      ["".join([card.split("\n")[index][:card_width] + " " * max(card_width - 7, 0)
    #             #                                 for card in cards[:-1]])
    #             #                         + cards[-1].split("\n")[index]
    #             #                                         for index in range(7)]))
    #         except IndexError:
    #             # No cards to be displayed
    #             repr_string = "\n".join([" " * self.maxwidth] * 7)
    #     elif style == "vertical":
    #         try:
    #             repr_string = "\n".join([l for c in cards[:-1] for l in str(c).split("\n")[:4]]) + "\n" + cards[-1]
    #         except IndexError:
    #             repr_string = "\n".join([" " * 7] * 7)
    #     elif style == "hidden":
    #         # Print a single box with the player's name and the number of cards in the
    #         # player's hand in it.
    #         top = u"\u256d" + u"\u2500"*5 + u"\u256e\n"
    #         bottom = u"\u2570"+ u"\u2500"*5 + u"\u256f"
            # interior = (u"\u2502" + u"/" * 5 + u"\u2502\n"
            #             + u"\u2502" + "{:/^5}".format(self.name) + u"\u2502\n"
            #             + u"\u2502" + "/" + "{:/>2}".format(len(self)) + "//" + u"\u2502\n"
            #             + (u"\u2502" + "/" * 5 + u"\u2502\n") * 2 )
    #         repr_string = top + interior + bottom
    #     elif style == "top":
    #         repr_string = str(self[-1])
    #     return repr_string
    # def __repr__(self):
    #     return self.repr(self.style)

# class Deck(list):
#     """
#     A Deck(n, s) is a shuffeled list of n copies of the cartesian product of
#     Deck.suites and ranks from s to 10 and "jack", "queen", "king" and "ace".
#     """
#     suites = ["spades", "hearts", "diamonds", "clubs"]
#     def __init__(self, n=1, start=7):
#         ranks = [str(k) for k in range(start, 11)] + ["jack", "queen", "king", "ace"]
#         base_deck = list(map(lambda c: Card(*c), list(product(ranks, Deck.suites)))) * n
#         shuffle(base_deck)
#         super().__init__(base_deck)
print("Hello, world!")

lambda: None
lambda x: x**2
lambda x, **y: x**2 + y

f(f(f(f(f(f(f(f(f(g(h(i(j(k(x))))))))))))))

f"abc"
u"def"
r"raw"
fr"abc"# A comment

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
    
for _ in range(3):
    print("Foo")

#!/usr/bin/python3

from cardgames import Card, Hand, Deck
from itertools import cycle, zip_longest
import time
import os

class Player(object):
    def __init__(self, game, name):
        self.game = game
        self.name = name
        self.cards = Hand()
    def get_card(self):
        self.cards.append(self.game.deck.pop())

class Pointeur(Player):
    def __repr__(self):
        return self.cards.repr(style="vertical")

class Croupier(Player):
    def __init__(self, game, name):
        super().__init__(game, name)
        self.cards = Hand(maxwidth=11)
    def __repr__(self):
        return self.cards.repr(style="horizontal")

class HumanPlayer(Pointeur):
    def __init__(self, game, name):
        super().__init__(game, name)

class AIPlayer(Pointeur):
    def __init__(self, game, name):
        super().__init__(game, name)

class Game(object):
    def __init__(self, players=3):
        self.deck = Deck(n=6, start=2)
        self.croupier = Croupier(self, "")
        self.player_list = [AIPlayer(self, "") for _ in range(players)] + [self.croupier]
        self.players = cycle(self.player_list)
        for _ in range(2):
            for _ in range(players + 1):
                time.sleep(1)
                self.current_player = next(self.players)
                self.current_player.get_card()
                print(self)
    def __repr__(self):
        anchor = "\x1b7\x1b[1;1f" # ANSI escape sequence to start at row 1, columm 1
        width = 13 * (len(self.player_list) - 1) - 5
        top = "\n".join(map(lambda arg: "{:^{width}}".format(arg, width=width), str(self.croupier).splitlines()))
        bottom = "\n".join(map((" " * 5).join, zip_longest(*[str(p).splitlines() for p in self.player_list[:-1]], fillvalue=" " * 7)))
        return anchor + top + "\n" * 5 + bottom + "\x1b8"

if __name__ == "__main__":
    os.system("clear")
    g = Game()
from itertools import product
from random import shuffle

class Card(object):
    """
    A Card is made up of a suite and a rank, and a fancy unicode __repr__ string.
    """
    symbols = {"spades": u"\u2660",
               "hearts": u"\u2665",
               "diamonds": u"\u2666",
               "clubs": u"\u2663"}
    def __init__(self, rank, suite):
        self.rank = rank
        self.suite = suite
    def __repr__(self):
        """
        Paint a card with the rank in the top left and bottom right corner
        and the suite symbol printed in the middle.
        """
        bottom = u"\u2570"+ u"\u2500"*5 + u"\u256f"
        if len(self.rank) < 3:
            rank = self.rank
        else:
            rank = self.rank[0].upper()
        top = u"\u256d" + u"\u2500"*5 + u"\u256e\n"
        interior = (u"\u2502" + "{:<2}".format(rank) + " " * 3 + u"\u2502\n"
                    + u"\u2502" + "     " + u"\u2502\n"
                    + u"\u2502" + "  " + Card.symbols[self.suite] + "  " + u"\u2502\n"
                    + u"\u2502" + "     " + u"\u2502\n"
                    + u"\u2502" + " " * 3 + "{:>2}".format(rank) + u"\u2502\n")
        return top + interior + bottom
    HIDDEN = u"\u256d" + u"\u2500"*5 + u"\u256e\n"  + (u"\u2502" + "/////" + u"\u2502\n") * 5 + u"\u2570"+ u"\u2500"*5 + u"\u256f"

class Hand(list):
    """
    A Hand() object is just a list of Card() objects, which additionally can be
    addressed by an alphabetical index in order for the user to be able to
    select a card with a single key press.
    """
    alphabet = "123456789abcdefghijklmnopqrstuvwxyz"
    def __init__(self, *args, style="horizontal", name="", maxwidth=59, **kwargs):
        self.style = style
        self.name = name
        self.maxwidth = maxwidth
        super().__init__(*args, **kwargs)
    def __call__(self, index):
        position = Hand.alphabet.find(index.lower())
        if position > -1:
            return self[position]
        else:
            raise IndexError
    # def repr(self, style="horizontal", hide=["1"], card_width=8):
    #     cards = []
    #     for index, card in zip(Hand.alphabet, self):
    #         if index in hide:
    #             cards.append(Card.HIDDEN)
    #         else:
    #             cards.append(str(card))
    #     if style == "horizontal":
    #         try:
    #             repr_string = "Foo"
    #             # repr_string = "\n".join(map(lambda arg: "{:<{maxwidth}}".format(arg, maxwidth=self.maxwidth),
    #             #                      ["".join([card.split("\n")[index][:card_width] + " " * max(card_width - 7, 0)
    #             #                                 for card in cards[:-1]])
    #             #                         + cards[-1].split("\n")[index]
    #             #                                         for index in range(7)]))
    #         except IndexError:
    #             # No cards to be displayed
    #             repr_string = "\n".join([" " * self.maxwidth] * 7)
    #     elif style == "vertical":
    #         try:
    #             repr_string = "\n".join([l for c in cards[:-1] for l in str(c).split("\n")[:4]]) + "\n" + cards[-1]
    #         except IndexError:
    #             repr_string = "\n".join([" " * 7] * 7)
    #     elif style == "hidden":
    #         # Print a single box with the player's name and the number of cards in the
    #         # player's hand in it.
    #         top = u"\u256d" + u"\u2500"*5 + u"\u256e\n"
    #         bottom = u"\u2570"+ u"\u2500"*5 + u"\u256f"
            # interior = (u"\u2502" + u"/" * 5 + u"\u2502\n"
            #             + u"\u2502" + "{:/^5}".format(self.name) + u"\u2502\n"
            #             + u"\u2502" + "/" + "{:/>2}".format(len(self)) + "//" + u"\u2502\n"
            #             + (u"\u2502" + "/" * 5 + u"\u2502\n") * 2 )
    #         repr_string = top + interior + bottom
    #     elif style == "top":
    #         repr_string = str(self[-1])
    #     return repr_string
    # def __repr__(self):
    #     return self.repr(self.style)

# class Deck(list):
#     """
#     A Deck(n, s) is a shuffeled list of n copies of the cartesian product of
#     Deck.suites and ranks from s to 10 and "jack", "queen", "king" and "ace".
#     """
#     suites = ["spades", "hearts", "diamonds", "clubs"]
#     def __init__(self, n=1, start=7):
#         ranks = [str(k) for k in range(start, 11)] + ["jack", "queen", "king", "ace"]
#         base_deck = list(map(lambda c: Card(*c), list(product(ranks, Deck.suites)))) * n
#         shuffle(base_deck)
#         super().__init__(base_deck)
print("Hello, world!")

lambda: None
lambda x: x**2
lambda x, **y: x**2 + y

f(f(f(f(f(f(f(f(f(g(h(i(j(k(x))))))))))))))

f"abc"
u"def"
r"raw"
fr"abc"# A comment

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
    
for _ in range(3):
    print("Foo")

#!/usr/bin/python3

from cardgames import Card, Hand, Deck
from itertools import cycle, zip_longest
import time
import os

class Player(object):
    def __init__(self, game, name):
        self.game = game
        self.name = name
        self.cards = Hand()
    def get_card(self):
        self.cards.append(self.game.deck.pop())

class Pointeur(Player):
    def __repr__(self):
        return self.cards.repr(style="vertical")

class Croupier(Player):
    def __init__(self, game, name):
        super().__init__(game, name)
        self.cards = Hand(maxwidth=11)
    def __repr__(self):
        return self.cards.repr(style="horizontal")

class HumanPlayer(Pointeur):
    def __init__(self, game, name):
        super().__init__(game, name)

class AIPlayer(Pointeur):
    def __init__(self, game, name):
        super().__init__(game, name)

class Game(object):
    def __init__(self, players=3):
        self.deck = Deck(n=6, start=2)
        self.croupier = Croupier(self, "")
        self.player_list = [AIPlayer(self, "") for _ in range(players)] + [self.croupier]
        self.players = cycle(self.player_list)
        for _ in range(2):
            for _ in range(players + 1):
                time.sleep(1)
                self.current_player = next(self.players)
                self.current_player.get_card()
                print(self)
    def __repr__(self):
        anchor = "\x1b7\x1b[1;1f" # ANSI escape sequence to start at row 1, columm 1
        width = 13 * (len(self.player_list) - 1) - 5
        top = "\n".join(map(lambda arg: "{:^{width}}".format(arg, width=width), str(self.croupier).splitlines()))
        bottom = "\n".join(map((" " * 5).join, zip_longest(*[str(p).splitlines() for p in self.player_list[:-1]], fillvalue=" " * 7)))
        return anchor + top + "\n" * 5 + bottom + "\x1b8"

if __name__ == "__main__":
    os.system("clear")
    g = Game()
from itertools import product
from random import shuffle

class Card(object):
    """
    A Card is made up of a suite and a rank, and a fancy unicode __repr__ string.
    """
    symbols = {"spades": u"\u2660",
               "hearts": u"\u2665",
               "diamonds": u"\u2666",
               "clubs": u"\u2663"}
    def __init__(self, rank, suite):
        self.rank = rank
        self.suite = suite
    def __repr__(self):
        """
        Paint a card with the rank in the top left and bottom right corner
        and the suite symbol printed in the middle.
        """
        bottom = u"\u2570"+ u"\u2500"*5 + u"\u256f"
        if len(self.rank) < 3:
            rank = self.rank
        else:
            rank = self.rank[0].upper()
        top = u"\u256d" + u"\u2500"*5 + u"\u256e\n"
        interior = (u"\u2502" + "{:<2}".format(rank) + " " * 3 + u"\u2502\n"
                    + u"\u2502" + "     " + u"\u2502\n"
                    + u"\u2502" + "  " + Card.symbols[self.suite] + "  " + u"\u2502\n"
                    + u"\u2502" + "     " + u"\u2502\n"
                    + u"\u2502" + " " * 3 + "{:>2}".format(rank) + u"\u2502\n")
        return top + interior + bottom
    HIDDEN = u"\u256d" + u"\u2500"*5 + u"\u256e\n"  + (u"\u2502" + "/////" + u"\u2502\n") * 5 + u"\u2570"+ u"\u2500"*5 + u"\u256f"

class Hand(list):
    """
    A Hand() object is just a list of Card() objects, which additionally can be
    addressed by an alphabetical index in order for the user to be able to
    select a card with a single key press.
    """
    alphabet = "123456789abcdefghijklmnopqrstuvwxyz"
    def __init__(self, *args, style="horizontal", name="", maxwidth=59, **kwargs):
        self.style = style
        self.name = name
        self.maxwidth = maxwidth
        super().__init__(*args, **kwargs)
    def __call__(self, index):
        position = Hand.alphabet.find(index.lower())
        if position > -1:
            return self[position]
        else:
            raise IndexError
    # def repr(self, style="horizontal", hide=["1"], card_width=8):
    #     cards = []
    #     for index, card in zip(Hand.alphabet, self):
    #         if index in hide:
    #             cards.append(Card.HIDDEN)
    #         else:
    #             cards.append(str(card))
    #     if style == "horizontal":
    #         try:
    #             repr_string = "Foo"
    #             # repr_string = "\n".join(map(lambda arg: "{:<{maxwidth}}".format(arg, maxwidth=self.maxwidth),
    #             #                      ["".join([card.split("\n")[index][:card_width] + " " * max(card_width - 7, 0)
    #             #                                 for card in cards[:-1]])
    #             #                         + cards[-1].split("\n")[index]
    #             #                                         for index in range(7)]))
    #         except IndexError:
    #             # No cards to be displayed
    #             repr_string = "\n".join([" " * self.maxwidth] * 7)
    #     elif style == "vertical":
    #         try:
    #             repr_string = "\n".join([l for c in cards[:-1] for l in str(c).split("\n")[:4]]) + "\n" + cards[-1]
    #         except IndexError:
    #             repr_string = "\n".join([" " * 7] * 7)
    #     elif style == "hidden":
    #         # Print a single box with the player's name and the number of cards in the
    #         # player's hand in it.
    #         top = u"\u256d" + u"\u2500"*5 + u"\u256e\n"
    #         bottom = u"\u2570"+ u"\u2500"*5 + u"\u256f"
            # interior = (u"\u2502" + u"/" * 5 + u"\u2502\n"
            #             + u"\u2502" + "{:/^5}".format(self.name) + u"\u2502\n"
            #             + u"\u2502" + "/" + "{:/>2}".format(len(self)) + "//" + u"\u2502\n"
            #             + (u"\u2502" + "/" * 5 + u"\u2502\n") * 2 )
    #         repr_string = top + interior + bottom
    #     elif style == "top":
    #         repr_string = str(self[-1])
    #     return repr_string
    # def __repr__(self):
    #     return self.repr(self.style)

# class Deck(list):
#     """
#     A Deck(n, s) is a shuffeled list of n copies of the cartesian product of
#     Deck.suites and ranks from s to 10 and "jack", "queen", "king" and "ace".
#     """
#     suites = ["spades", "hearts", "diamonds", "clubs"]
#     def __init__(self, n=1, start=7):
#         ranks = [str(k) for k in range(start, 11)] + ["jack", "queen", "king", "ace"]
#         base_deck = list(map(lambda c: Card(*c), list(product(ranks, Deck.suites)))) * n
#         shuffle(base_deck)
#         super().__init__(base_deck)
print("Hello, world!")

lambda: None
lambda x: x**2
lambda x, **y: x**2 + y

f(f(f(f(f(f(f(f(f(g(h(i(j(k(x))))))))))))))

f"abc"
u"def"
r"raw"
fr"abc"# A comment

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
    
for _ in range(3):
    print("Foo")

