import pulp
import argparse
import sys
import re
from typing import List
from dataclasses import dataclass


@dataclass
class Machine:
    lights: list[bool]
    buttons: list[list[int]]
    joltages: list[int]


def parse(line: str) -> Machine:
    pattern = r"^\[(?P<lights>[.#]+)\]\s+(?P<buttons>(?:\([\d,\s]+\)\s*)+)\s+\{(?P<joltages>[\d,\s,]+)\}$"
    match = re.match(pattern, line.strip())
    return Machine(
        lights=[char == "#" for char in match.group("lights")],
        buttons=[
            [int(x) for x in m.split(",")]
            for m in re.findall(r"\(([^)]+)\)", match.group("buttons"))
        ],
        joltages=[int(x) for x in match.group("joltages").split(",")],
    )


def configureLights(idx: int, machine: Machine) -> int:
    problem = pulp.LpProblem("lights-machine-%s" % idx, pulp.LpMinimize)
    buttons = [
        pulp.LpVariable(f"button_{i}", cat="Binary")
        for i in range(len(machine.buttons))
    ]
    for i, target in enumerate(machine.lights):
        lights = []
        for j, button in enumerate(machine.buttons):
            if i in button:
                lights.append(buttons[j])
        omega = pulp.LpVariable(f"omega_{i}", lowBound=0, cat="Integer")
        problem += pulp.lpSum(lights) == ((1 if target else 0) + 2 * omega)

    problem += pulp.lpSum(buttons)
    solver = pulp.PULP_CBC_CMD(msg=False)
    status = problem.solve(solver)
    return sum([int(pulp.value(v)) for v in buttons])


def configureJoltages(idx: int, machine: Machine) -> int:
    problem = pulp.LpProblem("joltages-machine-%s" % idx, pulp.LpMinimize)

    upperBound = max(machine.joltages)
    buttons = []
    for i, button in enumerate(machine.buttons):
        name = f"button_{i}"
        var = pulp.LpVariable(name, lowBound=0, upBound=upperBound, cat="Integer")
        buttons.append(var)

    for i, joltage in enumerate(machine.joltages):
        joltages = []
        for j, button in enumerate(machine.buttons):
            if i in button:
                joltages.append(buttons[j])
        problem += (pulp.lpSum(joltages) == joltage), f"joltage_{i}"

    problem += pulp.lpSum(buttons)
    solver = pulp.PULP_CBC_CMD(msg=False)
    status = problem.solve(solver)
    return sum([int(pulp.value(v)) for v in buttons])


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("filepath", type=str, help="path to input file")
    args = parser.parse_args()
    machines = []
    try:
        with open(args.filepath, "r") as file:
            for i, line in enumerate(file):
                if not line.strip():
                    continue
                try:
                    machine = parse(line)
                    print(f"{machine}")
                    machines.append(machine)
                except Exception as e:
                    print(f"{e}")
                    sys.exit(1)
    except FileNotFoundError:
        sys.exit(1)

    score1 = sum(list(map(lambda m: configureLights(m[0], m[1]), enumerate(machines))))
    score2 = sum(
        list(map(lambda m: configureJoltages(m[0], m[1]), enumerate(machines)))
    )
    print(f"Part 1: {score1}")
    print(f"Part 2: {score2}")


if __name__ == "__main__":
    main()
