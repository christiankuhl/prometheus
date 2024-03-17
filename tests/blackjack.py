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
