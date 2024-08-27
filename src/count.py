import pandas as pd
import numpy as np

from tree import Tree
from reader import COMBINATORS


def count_combinators(input_path: str, output_path: str) -> None:
    with open(input_path, "r") as input:
        text: str = input.read()
        counts: dict[str, int] = {combinator: 0 for combinator in COMBINATORS}
        for combinator in COMBINATORS:
            counts[combinator] = text.count(combinator)
    counts_sorted: list[tuple[str, int]] = sorted(
        counts.items(), key=lambda i: i[1], reverse=True
    )
    with open(output_path, "w") as output:
        for counts in counts_sorted:
            print(f"{counts[0]}: {counts[1]}", file=output)


class CompositionCount:
    def __init__(self):
        self.combs_lst: list[str] = []

    def traverse(self, node: Tree) -> None:
        if node.is_terminal:
            self.combs_lst.append(node.comb)
        else:
            if node.is_unary:
                self.traverse(node.child)
                self.combs_lst.append(node.comb)
            else:
                self.traverse(node.left)
                self.traverse(node.right)
                self.combs_lst.append(node.comb)

    @staticmethod
    def make_csv(trees: list[Tree], output_path: str, unified_df) -> None:
        words: list[str] = []
        self = CompositionCount()
        for tree in trees:
            self.traverse(tree)
            words += tree.tokens
        stack: list[str] = ["lex"]
        output_list: list[list[str]] = []
        for i in self.combs_lst:
            if i == "lex":
                output_list.append(stack)
                stack = ["lex"]
            else:
                stack.append(i)
        output_list.append(stack)
        output_list = output_list[1:]

        app_lst = []
        comp_lst = []
        tr_lst = []
        others_lst = []
        nd_lst = []
        app = 0
        comp = 0
        tr = 0
        others = 0
        nd = 0
        for comb_lst in output_list:
            for comb in comb_lst:
                comb_type = clasify_combs(comb)
                match comb_type:
                    case "App":
                        app += 1
                    case "Comp":
                        comp += 1
                    case "TR":
                        tr += 1
                    case "Others":
                        others += 1
                    case "lex":
                        continue
            nd = app + comp + tr + others
            app_lst.append(app)
            comp_lst.append(comp)
            tr_lst.append(tr)
            others_lst.append(others)
            nd_lst.append(nd)
            app = 0
            comp = 0
            tr = 0
            others = 0
            nd = 0

        app_count = np.array(app_lst)
        comp_count = np.array(comp_lst)
        tr_count = np.array(tr_lst)
        others_count = np.array(others_lst)
        nd_count = np.array(nd_lst)
        words = np.array(words)
        df = pd.DataFrame(
            np.stack(
                [words, app_count, comp_count, tr_count, others_count, nd_count], 1
            ),
            columns=["token", "app", "comp", "tr", "others", "nodecount"],
        )

        for col in ["app", "comp", "tr", "others", "nodecount", "num_of_words"]:
            unified_df[col] = 0

        idx = 0
        for _, row in unified_df.iterrows():
            surface = row["surface"]
            tokens = []
            app_sum = 0
            comp_sum = 0
            tr_sum = 0
            others_sum = 0
            nodecount_sum = 0

            while "".join(tokens) != surface and idx < len(df):
                tokens.append(df.iloc[idx]["token"])
                app_sum += int(df.iloc[idx]["app"])
                comp_sum += int(df.iloc[idx]["comp"])
                tr_sum += int(df.iloc[idx]["tr"])
                others_sum += int(df.iloc[idx]["others"])
                nodecount_sum += int(df.iloc[idx]["nodecount"])
                idx += 1

            if "".join(tokens) == surface:
                unified_df.at[_, "app"] = app_sum
                unified_df.at[_, "comp"] = comp_sum
                unified_df.at[_, "tr"] = tr_sum
                unified_df.at[_, "others"] = others_sum
                unified_df.at[_, "nodecount"] = nodecount_sum
                unified_df.at[_, "num_of_words"] = len(tokens)

        unified_df.to_csv(output_path, index=False)


def clasify_combs(comb: str) -> str:
    if comb in {">", "<"}:
        return "App"
    elif "T" in comb:
        return "TR"
    elif "B" in comb:
        return "Comp"
    elif comb == "lex":
        return "lex"
    else:
        return "Others"
