# This script is based on https://github.com/masashi-y/depccg/blob/master/depccg/tools/reader.py

from typing import Iterator, Optional
from tree import Tree, printer
from category import Category
from grammar import binary_comp, PUNC, CONJ

import logging

logger = logging.getLogger(__name__)

COMBINATORS: set[str] = {
    ">",
    "<",
    ">B",
    ">B2",
    ">B3",
    ">Bx1",
    ">Bx2",
    ">Bx3",
    ">Bx4",
    "<B1",
    "<B2",
    "<B3",
    "<Bx",
    "<Bx2",
    "<Bx3",
    "punc",
    "glue",
    "NM",
    ">T",
    "<T",
    "TC",
    "TC2",
    "ADNint",
    "ADV0",
    "ADN",
    "NM",
    "ADV",
}


stack: list[Tree] = []


class AutoLineReader:
    def __init__(self, line: str):
        self.line: str = line
        self.index: int = 0
        self.word_id: int = -1
        self.binary_comp = binary_comp
        self.tokens: list[str] = []

    def _next(self) -> str:
        end: int = self.line.find(" ", self.index)
        res: str = self.line[self.index : end]
        self.index = end + 1
        return res

    def _check(self, text: str, offset=0) -> None:
        if self.line[self.index + offset] != text:
            raise RuntimeError(f"failed to parse: {self.line}")

    def _peek(self) -> str:
        return self.line[self.index]

    def parse(self) -> Tree:
        tree = self.next_node()
        return tree

    @property
    def _next_node(self):
        if self.line[self.index + 2] == "L":
            return self._parse_leaf
        elif self.line[self.index + 2] == "T":
            return self._parse_tree
        else:
            raise RuntimeError(
                f"failed to parse:\n{self.index + 2=}\n{self.line[self.index + 2]=}\n{self.line=}"
            )

    def _parse_leaf(self) -> Tree:
        self.word_id += 1
        self._check("(")
        self._check("<", 1)
        self._check("L", 2)
        self._next()
        cat: Category = Category.from_string(self._next())
        self._next()
        self._next()
        # tag1 = self.next()  # modified POS tag
        # tag2 = self.next()  # original POS
        token: str = self._next().replace("\\", "")
        if token == "(":
            token = "LRB"
        elif token == ")":
            token = "RRB"
        self.tokens.append(token)
        self._next()
        return Tree(cat, None, "lex", token)

    def parse_tree(self) -> Tree:
        self._check("(")
        self._check("<", 1)
        self._check("T", 2)
        self._next()
        cat: Category = Category.from_string(self._next())
        self._next()
        self._next()
        children: list[Tree] = []
        while self._peek() != ")":
            children.append(self._next_node())
        self._next()
        if len(children) == 2:
            left, right = children
            if left.cat == CONJ:
                return Tree(cat, [left, right], ">")
            if "conj" in right.cat.features:
                if left.cat.without_feature == right.cat.without_feature:
                    return Tree(cat, [left, right], "<")
                elif (
                    left.cat in PUNC
                    and right.cat.without_feature == cat.without_feature
                ):
                    return Tree(right.cat, [left, right], "punc")
                else:
                    if str(cat) == "GLUE":
                        return Tree(cat, [left, right], "glue")
                    raise ValueError(f"{printer(left)=}\n{printer(right)=}")
            new_tree: Optional[Tree] = Tree.comp(left, right)
            if new_tree:
                if new_tree.cat == cat:
                    return Tree(cat, [left, right], new_tree.comb)
                else:
                    stack.append(new_tree)
                    return Tree(cat, [left, right], new_tree.comb)
            else:
                if str(cat) == "GLUE":
                    return Tree(cat, [left, right], "glue")
                else:
                    stack.append(Tree(cat, [left, right], "TC2"))
                    return Tree(cat, [left, right], "TC2")

        elif len(children) == 1:
            if cat.without_feature == Category.from_string("NP") and children[
                0
            ].cat.without_feature == Category.from_string("N"):
                comb = "NM"
            elif cat.without_feature == Category.from_string("S/(S\\NP)") and children[
                0
            ].cat.without_feature == Category.from_string("NP"):
                comb = ">T"
            elif cat.without_feature == Category.from_string("NP\\NP") and children[
                0
            ].cat.without_feature == Category.from_string("S\\NP"):
                comb = "ADN"
            elif cat.without_feature == Category.from_string("N\\N") and children[
                0
            ].cat.without_feature == Category.from_string("S\\NP"):
                comb = "ADN"
            elif cat.without_feature == Category.from_string(
                "(S\\NP)\\(S\\NP)"
            ) and children[0].cat.without_feature == Category.from_string("S\\NP"):
                comb = "ADV"
            elif cat.without_feature == Category.from_string("S\\NP") and children[
                0
            ].cat.without_feature == Category.from_string("NP"):
                comb = "TC"
            elif cat.without_feature == Category.from_string("S\\NP") and children[
                0
            ].cat.without_feature == Category.from_string("N"):
                comb = "TC"
            elif cat.without_feature == Category.from_string("S\\NP") and children[
                0
            ].cat.without_feature == Category.from_string("S/(S/NP)"):
                comb = "TC"
            elif cat.without_feature == Category.from_string("S\\NP") and children[
                0
            ].cat.without_feature == Category.from_string("NP\\NP"):
                comb = "TC"
            elif cat.without_feature == Category.from_string("S\\NP") and children[
                0
            ].cat.without_feature == Category.from_string("N\\N"):
                comb = "TC"
            elif cat.without_feature == Category.from_string("S\\NP") and children[
                0
            ].cat.without_feature == Category.from_string("S/(S\\NP)"):
                comb = "TC"
            elif cat.without_feature == Category.from_string("S\\NP") and children[
                0
            ].cat.without_feature == Category.from_string("(S\\NP)\\(S\\NP)"):
                comb = "TC"
            elif cat.without_feature == Category.from_string("NP") and children[
                0
            ].cat.without_feature == Category.from_string("(S\\NP)\\(S\\NP)"):
                comb = "TC"
            elif cat.without_feature == Category.from_string("N") and children[
                0
            ].cat.without_feature == Category.from_string("(S\\NP)\\(S\\NP)"):
                comb = "TC"
            elif cat.without_feature == Category.from_string("S/NP") and children[
                0
            ].cat.without_feature == Category.from_string("N\\N"):
                comb = "TC"
            else:
                comb = "TC"
                print(f"{str(children[0].cat)=}")
                print(f"{str(cat)=}")
            return Tree(cat, children, comb)
        else:
            raise RuntimeError(f"failed to parse:\n{children=}\n{self.line=}")


def read_auto(filename: str) -> Iterator[Tree]:
    for line in open(filename):
        line = line.strip()
        if len(line) == 0:
            continue
        tree = AutoLineReader(line).parse()
        yield tree


def read_parsedJaTree(filepath: str) -> Iterator[Tree]:
    with open(filepath, "r") as f:
        for line in f:
            line = line.strip()
            if len(line) == 0:
                continue
            tree = JaReader(line).parse()
            yield tree


def read_parsedJaString(strings: list[str]) -> Iterator[Tree]:
    for line in strings:
        tree = JaReader(line).parse()
        yield tree


class JaReader:
    def __init__(self, line: str) -> None:
        self.line = line
        self.index = 0
        self.word_id = -1
        self.tokens = []

    def _next(self, target: str) -> str:
        end = self.line.find(
            target, self.index
        )  # end = targetの文字列が現れる最初のindex
        result = self.line[self.index : end]
        self.index = end + 1
        return result

    def _is_current_idx(self, text: str, offset: int = 0) -> None:
        if self.line[self.index + offset] != text:
            raise RuntimeError("the position of 'index' is not correct")

    def _peek_current_idx_str(self) -> str:
        return self.line[self.index]

    def parse(self) -> Tree:
        result = self._next_node()
        return result

    @property
    def _next_node(self):
        end = self.line.find(" ", self.index)  # end = " "が現れるindex
        if self.line[self.index + 1 : end] in COMBINATORS:
            return self._parse_tree
        else:
            return self._parse_terminal

    def _parse_terminal(self) -> Tree:
        self.word_id += 1
        self._is_current_idx("{")
        cat = self._next(" ")[1:]
        cat = Category.from_string(cat)
        token = self._next("}")
        if token.count("/") == 3:
            token, _, _, _ = token.split("/")
        else:
            token = "/".join(token.split("/")[:-3])
        return Tree(cat, None, "lex", token)

    def _parse_tree(self) -> Tree:
        self._is_current_idx("{")
        comb = self._next(" ")[1:]
        cat = self._next(" ")
        cat = Category.from_string(cat)
        self._is_current_idx("{")

        children = []
        while self._peek_current_idx_str() != "}":
            children.append(self._next_node())
            if self._peek_current_idx_str() == " ":
                self._next(" ")

        self._next("}")

        if len(children) == 1:
            return Tree(cat, [children[0]], comb)
        else:
            assert (
                len(children) == 2
            ), f"failed to parse, invalid number of children: {self.line}"
            left, right = children
            return Tree(cat, [left, right], comb)
