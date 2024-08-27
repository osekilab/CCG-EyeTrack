# This script is based on https://github.com/masashi-y/depccg/blob/master/depccg/tree.py


import re
from typing import Optional

from category import Category, Complex
from grammar import binary_comp, ba

# constraint
ROOT_CATS: set[Category] = {
    Category.from_string("NP[nc]"),
    Category.from_string("NP"),
    Category.from_string("S"),
}


class Tree:
    def __init__(
        self,
        cat: Category,
        children: Optional[list["Tree"]],
        comb: str = "lex",
        token: Optional[str] = None,
    ) -> None:
        assert children != token, "両方Noneはだめ"
        self.cat = cat
        self.children = children
        self.comb = comb
        self.token = token

    @property
    def leaves(self) -> list["Tree"]:
        def rec(tree: "Tree") -> None:
            assert isinstance(tree, Tree), f"{tree=}"
            if tree.children:
                for child in tree.children:
                    rec(child)
            else:
                result.append(tree)

        result: list["Tree"] = []
        rec(self)
        return result

    @property
    def tokens(self) -> list[str]:
        return [leaf.token for leaf in self.leaves if isinstance(leaf.token, str)]

    @property
    def rightmost_token(self) -> str:
        if self.is_terminal:
            return self.token
        return self.tokens[-1]

    @property
    def terminal_cat(self) -> list[str]:
        return [str(leaf.cat) for leaf in self.leaves]

    @property
    def terminal_token_cat(self) -> list[tuple[str, Category]]:
        return [
            (leaf.token, leaf.cat)
            for leaf in self.leaves
            if isinstance(leaf.token, str)
        ]

    @property
    def is_terminal(self) -> bool:
        return isinstance(self.token, str)

    @property
    def word(self) -> str:
        return " ".join(token for token in self.tokens)

    @property
    def left(self) -> "Tree":
        assert self.is_binary
        assert self.children
        return self.children[0]

    @property
    def right(self) -> "Tree":
        assert self.is_binary
        assert self.children
        return self.children[1]

    @property
    def child(self) -> "Tree":
        assert self.is_unary
        assert self.children
        return self.children[0]

    @property
    def is_unary(self) -> bool:
        return isinstance(self.children, list) and len(self.children) == 1

    @property
    def is_binary(self) -> bool:
        return isinstance(self.children, list) and len(self.children) == 2

    @property
    def is_root(self) -> bool:
        return self.cat in ROOT_CATS

    @staticmethod
    def comp(left: Optional["Tree"], right: Optional["Tree"]) -> Optional["Tree"]:
        if left and right:
            cat, comb = binary_comp(left.cat, right.cat)
            if cat and comb:
                return Tree(cat, [left, right], comb)
        return


def typeraise(left: Category, right: Complex) -> Complex:
    return Complex(right.left, "/", Complex(right.left, "\\", left))


def apply_typeraise(tree: Tree) -> Tree:
    def _apply_typeraise(node: Tree) -> Tree:
        if node.is_terminal:
            return Tree(node.cat, None, "lex", node.token)
        elif node.is_unary:
            return Tree(
                node.cat,
                [_apply_typeraise(node.child)],
                node.comb,
            )
        elif (
            node.comb == "<"
            and re.match(r"(\(*)S", str(node.right.cat)) is not None
            and str(node.right.cat).count("S") == 1
        ):
            return Tree(
                node.cat,
                [
                    Tree(
                        typeraise(node.left.cat, node.right.cat),
                        [_apply_typeraise(node.left)],
                        ">T",
                    ),
                    _apply_typeraise(node.right),
                ],
                ">",
            )
        else:
            return Tree(
                node.cat,
                [_apply_typeraise(node.left), _apply_typeraise(node.right)],
                node.comb,
            )

    return _apply_typeraise(tree)


def en_apply_typeraise(tree: Tree) -> Tree:
    def _apply_typeraise(node: Tree) -> Tree:
        if node.is_terminal:
            return Tree(node.cat, None, "lex", node.token)
        elif node.is_unary:
            return Tree(
                node.cat,
                [_apply_typeraise(node.child)],
                node.comb,
            )
        elif node.comb == "<" and ba(node.left.cat, node.right.cat):
            return Tree(
                node.cat,
                [
                    Tree(
                        typeraise(node.left.cat, node.right.cat),
                        [_apply_typeraise(node.left)],
                        ">T",
                    ),
                    _apply_typeraise(node.right),
                ],
                ">",
            )
        else:
            return Tree(
                node.cat,
                [_apply_typeraise(node.left), _apply_typeraise(node.right)],
                node.comb,
            )

    return _apply_typeraise(tree)


def rotate2left(tree: Tree) -> Tree:
    def _rotate2left(node: Tree) -> Tree:
        if node.is_terminal:
            return Tree(node.cat, None, "lex", node.token)
        elif node.is_unary:
            return Tree(node.cat, [_rotate2left(node.children[0])], node.comb)
        else:  # node.is_binary
            if node.right.is_binary:
                new_node: Optional[Tree] = Tree.comp(
                    Tree.comp(node.left, node.right.left), node.right.right
                )
                if new_node:
                    return _rotate2left(new_node)
                return Tree(
                    node.cat,
                    [_rotate2left(node.left), _rotate2left(node.right)],
                    node.comb,
                )
            return Tree(
                node.cat,
                [_rotate2left(node.left), _rotate2left(node.right)],
                node.comb,
            )

    return _rotate2left(tree)


def printer(tree: Tree) -> str:
    # Output tree strings of the Japanese CCGBank's format
    def _rec(node: Tree):
        if node.is_terminal:
            cat = node.cat
            word = node.word

            return f"{{{cat} {word}/_/_/_}}"
        else:
            children = " ".join(_rec(child) for child in node.children)
            return f"{{{node.comb} {node.cat} {children}}}"

    return _rec(tree)
