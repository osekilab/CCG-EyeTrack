# This script is based on https://github.com/masashi-y/depccg/blob/master/depccg/cat.py

import copy
import re
from typing import Optional

SLASH = re.compile(r"([/\\])")
CAT_SPLIT = re.compile(r"([/\\]|[\[\]\(\)/\\])")


def inverse_dic(dictionary: dict):
    return {v: k for k, v in dictionary.items()}


class Feature:
    def __init__(self, value: Optional[str] = None):
        self.value: Optional[str] = value

    def __str__(self) -> str:
        return self.value if self.value else ""

    def __eq__(self, other: "Feature") -> bool:
        assert isinstance(
            other, Feature
        ), f"Feature is being compared with {type(object)}."
        return not self.value or not other.value or self.value == other.value

    def __hash__(self):
        return hash(self.value)


class Category:
    def __truediv__(self, other: "Category") -> "Category":
        return Complex(self, "/", other)

    def __or__(self, other: "Category") -> "Category":
        return Complex(self, "\\", other)

    @property
    def is_complex(self):
        return not self.is_basic

    @property
    def is_basic(self):
        return not self.is_complex

    @classmethod
    def from_string(cls, txt: str) -> "Category":
        tokens = CAT_SPLIT.sub(r" \1 ", txt)
        buffer = list(reversed([i for i in tokens.split(" ") if i != ""]))
        stack = []

        while len(buffer):
            item = buffer.pop()
            if item in "(":
                stack.append(item)
            elif item in ")":
                y = stack.pop()
                assert len(stack) > 0
                if stack[-1] == "(" and item == ")":
                    assert stack.pop() in "("
                    stack.append(y)
                else:
                    f = stack.pop()
                    x = stack.pop()
                    assert stack.pop() in "("
                    stack.append(Complex(x, f, y))
            elif SLASH.match(item):
                stack.append(item)
            else:
                if len(buffer) >= 3 and buffer[-1] == "[":
                    buffer.pop()
                    feature = Feature(buffer.pop())
                    assert buffer.pop() == "]"
                    stack.append(Basic(item, feature))
                else:
                    stack.append(Basic(item))

        if len(stack) == 1:
            return stack[0]
        try:
            x, f, y = stack
            return Complex(x, f, y)
        except ValueError:
            raise RuntimeError(f"falied to parse category: {txt}")

    def clean_feature(self) -> None:
        def _rec(cat: Category) -> Category:
            if cat.is_complex:
                _rec(cat.left)
                _rec(cat.right)
            else:
                if isinstance(cat.feature, Feature):
                    cat.feature = None

        _rec(self)

    @property
    def without_feature(self) -> "Category":
        cat_copy = copy.deepcopy(self)

        def _rec(cat: Category) -> None:
            if cat.is_complex:
                _rec(cat.left)
                _rec(cat.right)
            else:
                if isinstance(cat.feature, Feature):
                    cat.feature = None

        _rec(cat_copy)
        return cat_copy

    @property
    def features(self) -> list[str]:
        def _rec(cat: Category) -> None:
            if cat.is_complex:
                _rec(cat.left)
                _rec(cat.right)
            else:
                if isinstance(cat.feature, Feature):
                    result.append(str(cat.feature))

        result: list[str] = []
        _rec(self)
        return result

    @property
    def is_vp(cat: "Category") -> bool:
        """
        An English functor category is a VP when
            1. it includes just one 'S' category and
            2. it starts with 'S' category and
        """
        s = str(cat)
        return cat.is_complex and s.count("S") == 1 and re.match(r"\(*S", s) is not None


class Basic(Category):
    def __init__(self, base: str, feature: Optional[Feature] = None):
        self.base: str = base
        self.feature: Optional[Feature] = feature
        self.t: Optional[Category] = None

    def __str__(self) -> str:
        if self.feature:
            return f"{self.base}[{self.feature}]"
        return self.base

    def __eq__(self, other: object) -> bool:
        if isinstance(other, str):
            other = Category.from_string(other)
        if isinstance(other, Complex):
            return False
        if self.feature and other.feature:
            return self.base == other.base and self.feature and other.feature
        else:
            return self.base == other.base

    def __xor__(self, other: object) -> bool:
        if not isinstance(other, Basic):
            return False
        return self.base == other.base

    def __hash__(self) -> int:
        return hash(str(self))

    @property
    def is_basic(self):
        return True

    @property
    def is_modifier(self) -> bool:
        return False

    @property
    def is_post_modifier(self) -> bool:
        return False

    @property
    def nargs(self) -> int:
        return 0

    @property
    def to_latex(self) -> str:
        if self.feature:
            return f"{self.base}\\[{self.feature}\\]"
        return self.base


class Complex(Category):
    def __init__(self, left: str | Category, slash: str, right: str | Category):
        self.left: Category = (
            Category.from_string(left) if isinstance(left, str) else left
        )
        self.slash: str = slash
        self.right: Category = (
            Category.from_string(right) if isinstance(right, str) else right
        )

    def __str__(self) -> str:
        def _str(cat):
            if isinstance(cat, Complex):
                return f"({cat})"
            return str(cat)

        return _str(self.left) + self.slash + _str(self.right)

    def __eq__(self, other: object) -> bool:
        if isinstance(other, str):
            other = Category.from_string(other)
        elif not isinstance(other, Complex):
            return False
        return (
            self.left == other.left
            and self.slash == other.slash
            and self.right == other.right
        )

    def __xor__(self, other: object) -> bool:
        if not isinstance(other, Complex):
            return False
        return (
            self.left ^ other.left
            and self.slash == other.slash
            and self.right ^ other.right
        )

    def __hash__(self) -> int:
        return hash(str(self))

    @property
    def is_complex(self):
        return True

    @property
    def is_modifier(self) -> bool:
        return self.left == self.right and self.slash == "/"

    @property
    def is_post_modifier(self) -> bool:
        return self.left == self.right and self.slash == "\\"

    @property
    def nargs(self) -> int:
        return 1 + self.left.nargs
