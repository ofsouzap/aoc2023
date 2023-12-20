from typing import List, Tuple, Optional, Dict
from abc import ABC, abstractmethod
from pathlib import Path
from collections import defaultdict
from itertools import count


# I give up with doing this in Haskell. I'm doing Python now for this one


INP = "day20.txt"
INP_TEST_A = "day20-testa.txt"
INP_TEST_B = "day20-testb.txt"


# Data


ModuleName = str
Signal = bool
WireSignal = Tuple[str, Signal]


class Module(ABC):
    @abstractmethod
    def get_name(self) -> ModuleName:
        pass
    @abstractmethod
    def get_outs(self) -> List[ModuleName]:
        pass
    @abstractmethod
    def signal(self, sig: Signal, src: ModuleName) -> Optional[Tuple[bool, List[ModuleName]]]:
        pass
    def __str__(self) -> str:
        return f"{self.get_name()}"
    def __repr__(self) -> str:
        return self.__str__()


class BroadcasterModule(Module):
    def __init__(self, targets: List[ModuleName]) -> None:
        self.targets: List[ModuleName] = targets
    def get_name(self) -> ModuleName:
        return "broadcaster"
    def get_outs(self) -> List[ModuleName]:
        return self.targets
    def signal(self, sig: Signal, src: ModuleName) -> Optional[Tuple[bool, List[ModuleName]]]:
        return (sig, self.targets)


class FFModule(Module):
    def __init__(self, name: ModuleName, targets: List[ModuleName]) -> None:
        self.name = name
        self.state = False
        self.targets: List[ModuleName] = targets
    def get_name(self) -> ModuleName:
        return self.name
    def get_outs(self) -> List[ModuleName]:
        return self.targets
    def signal(self, sig: Signal, src: ModuleName) -> Optional[Tuple[bool, List[ModuleName]]]:
        if not sig:
            self.state = not self.state
            return (self.state, self.targets)
        else:
            return None
    def __str__(self) -> str:
        return f"{self.get_name()} - {self.state}"


class ConjModule(Module):
    def __init__(self, name: ModuleName, targets: List[ModuleName]) -> None:
        self.name = name
        self.mem: Dict[ModuleName, bool] = {}
        self.targets: List[ModuleName] = targets
    def get_name(self) -> ModuleName:
        return self.name
    def get_outs(self) -> List[ModuleName]:
        return self.targets
    def signal(self, sig: Signal, src: ModuleName) -> Optional[Tuple[bool, List[ModuleName]]]:
        self.mem[src] = sig
        return (not all(self.mem.values()), self.targets)
    def __str__(self) -> str:
        return f"{self.get_name()} - " + ", ".join(f"{k}:{self.mem[k]}" for k in self.mem.keys())


def main_for(fn: str, parta: bool):

    # Reading Input

    with (Path(".") / "inputs" / fn).open("r") as file:
        lines = file.readlines()

    network: List[Module] = []

    for line in lines:
        parts = line.replace(" ", "").replace("\r", "").replace("\n", "").split("->")
        name = parts[0]
        targets = parts[1].split(",")
        if name[0] == "%":
            network.append(FFModule(name[1:], targets))
        elif name[0] == "&":
            network.append(ConjModule(name[1:], targets))
        elif name == "broadcaster":
            network.append(BroadcasterModule(targets))

    inps: defaultdict[ModuleName, List[ModuleName]] = defaultdict(lambda: [])

    for m in network:
        for o in m.get_outs():
            inps[o].append(m.get_name())

    for m in network:
        if isinstance(m, ConjModule):
            for inp in inps[m.get_name()]:
                m.mem[inp] = False

    # Doing the stuff

    q: List[Tuple[ModuleName, bool, List[ModuleName]]] = []
    lows_sent: int = 0
    highs_sent: int = 0
    presses: int = 0

    my_iterator = range(1000) if parta else count(1)
    stop_me = False

    for _ in my_iterator:

        if stop_me:
            break

        q.append(("button", False, ["broadcaster"]))
        presses += 1

        while q:

            src, sig, dsts = q.pop(0)

            for dst in dsts:
                if sig:
                    highs_sent += 1
                else:
                    lows_sent += 1

                if (dst == "rx") and (sig == False):
                    stop_me = True
                    break

                for m in network:
                    if m.get_name() == dst:
                        out = m.signal(sig, src)
                        if out is not None:
                            out_sig, out_dsts = out
                            q.append((dst, out_sig, out_dsts))
                        break

    # Output

    if parta:
        print(lows_sent * highs_sent)
    else:
        print(presses)


def main():
    main_for(INP, True)
    main_for(INP_TEST_A, True)
    main_for(INP_TEST_B, True)
    main_for(INP, False)


if __name__ == "__main__":
    main()
