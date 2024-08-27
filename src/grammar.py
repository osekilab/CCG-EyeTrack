import copy
from typing import Optional

from category import Category, Complex

PUNC: set[Category] = {
    Category.from_string("."),
    Category.from_string(","),
    Category.from_string(";"),
    Category.from_string(":"),
    Category.from_string("LRB"),
    Category.from_string("RRB"),
}
CONJ: Category = Category.from_string("conj")


def fa(left: Category, right: Category) -> Optional[Category]:
    if left.is_complex and left.right == right and left.slash == "/":
        left_deepcopy = copy.deepcopy(left)
        return left_deepcopy.left
    return


def ba(left: Category, right: Category) -> Optional[Category]:
    if right.is_complex and left == right.right and right.slash == "\\":
        right_deepcopy = copy.deepcopy(right)
        return right_deepcopy.left
    return


def fc(left: Category, right: Category) -> Optional[Category]:
    if (
        left.is_complex
        and right.is_complex
        and left.right == right.left
        and left.slash == right.slash == "/"
    ):
        return Complex(
            left.left,
            "/",
            right.right,
        )
    return


def fc2(left: Category, right: Category) -> Optional[Category]:
    if (
        left.is_complex
        and right.nargs >= 2
        and left.right == right.left.left
        and left.slash == right.left.slash == "/"
    ):
        result = copy.deepcopy(right)
        result.left.left = left.left
        return result
    return


def fc3(left: Category, right: Category) -> Optional[Category]:
    if (
        left.is_complex
        and right.nargs >= 3
        and left.right == right.left.left.left
        and left.slash == right.left.left.slash == "/"
    ):
        result = copy.deepcopy(right)
        result.left.left.left = left.left
        return result
    return


def fc4(left: Category, right: Category) -> Optional[Category]:
    if (
        left.is_complex
        and right.nargs >= 4
        and left.right == right.left.left.left.left
        and left.slash == right.left.left.left.slash == "/"
    ):
        result = copy.deepcopy(right)
        result.left.left.left.left = left.left
        return result
    return


def fcx(left: Category, right: Category) -> Optional[Category]:
    if (
        left.is_complex
        and right.is_complex
        and left.right == right.left
        and left.slash == "/"
        and right.slash == "\\"
    ):
        return Complex(
            left.left,
            "\\",
            right.right,
        )
    return


def fc2x(left: Category, right: Category) -> Optional[Category]:
    if (
        left.is_complex
        and right.nargs >= 2
        and left.right == right.left.left
        and left.slash == "/"
        and right.left.slash == "\\"
    ):
        result = copy.deepcopy(right)
        result.left.left = left.left
        return result
    return


def fc3x(left: Category, right: Category) -> Optional[Category]:
    if (
        left.is_complex
        and right.nargs >= 3
        and left.right == right.left.left.left
        and left.slash == "/"
        and right.left.left.slash == "\\"
    ):
        result = copy.deepcopy(right)
        result.left.left.left = left.left
        return result
    return


def fc4x(left: Category, right: Category) -> Optional[Category]:
    if (
        left.is_complex
        and right.nargs >= 4
        and left.right == right.left.left.left.left
        and left.slash == "/"
        and right.left.left.left.slash == "\\"
    ):
        result = copy.deepcopy(right)
        result.left.left.left.left = left.left
        return result
    return


def bc(left: Category, right: Category) -> Optional[Category]:
    if (
        left.is_complex
        and right.is_complex
        and left.left == right.right
        and right.slash == left.slash == "\\"
    ):
        return Complex(
            right.left,
            "\\",
            left.right,
        )
    return


def bc2(left: Category, right: Category) -> Optional[Category]:
    if (
        left.nargs >= 2
        and right.is_complex
        and left.left.left == right.right
        and right.slash == left.left.slash == "\\"
    ):
        result = copy.deepcopy(left)
        result.left.left = right.left
        return result
    return


def bc3(left: Category, right: Category) -> Optional[Category]:
    if (
        left.nargs >= 3
        and right.is_complex
        and left.left.left.left == right.right
        and right.slash == left.left.left.slash == "\\"
    ):
        result = copy.deepcopy(left)
        result.left.left.left = right.left
        return result
    return


def bcx(left: Category, right: Category) -> Optional[Category]:
    if (
        left.is_complex
        and right.is_complex
        and left.left == right.right
        and right.slash == "\\"
        and left.slash == "/"
    ):
        return Complex(
            right.left,
            "/",
            left.right,
        )
    return


def bc2x(left: Category, right: Category) -> Optional[Category]:
    if (
        left.nargs >= 2
        and right.is_complex
        and left.left.left == right.right
        and right.slash == "\\"
        and left.left.slash == "/"
    ):
        result = copy.deepcopy(left)
        result.left.left = right.left
        return result
    return


def bc3x(left: Category, right: Category) -> Optional[Category]:
    if (
        left.nargs >= 3
        and right.is_complex
        and left.left.left.left == right.right
        and right.slash == "\\"
        and left.left.left.slash == "/"
    ):
        result = copy.deepcopy(left)
        result.left.left.left = right.left
        return result
    return


def punc(left: Category, right: Category) -> Optional[Category]:
    if right in PUNC:
        return left
    elif left in PUNC:
        return right
    return


def conj(left: Category, right: Category) -> Optional[Category]:
    if left == CONJ:
        return right


COMBINATORS: dict = {
    fa: ">",
    ba: "<",
    fc: ">B",
    fc2: ">B2",
    fc3: ">B3",
    fc4: ">B4",
    fcx: ">Bx1",
    fc2x: ">Bx2",
    fc3x: ">Bx3",
    fc4x: ">Bx4",
    bc: "<B1",
    bc2: "<B2",
    bc3: "<B3",
    bcx: "<Bx",
    bc2x: "<Bx2",
    bc3x: "<Bx3",
    punc: "punc",
    conj: ">",  # corresponds to conj
}


def binary_comp(
    left: Category, right: Category
) -> tuple[Optional[Category], Optional[str]]:
    for combinator in COMBINATORS:
        cat: Optional[Category] = combinator(left, right)
        if cat:
            return cat, COMBINATORS[combinator]
        else:
            continue
    return None, None
