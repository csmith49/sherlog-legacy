from collections import defaultdict
import pyro
from . import statement

class Model:
    def __init__(self, statements):
        self.statements = statements

        self.variable_indices = {s.variable.name : i for i, s in enumerate(self.statements)}

        self.dependency_graph = defaultdict(lambda: [])
        self.dataflow_graph = defaultdict(lambda: [])

        for stmt in self.statements:
            source = self.variable_indices[stmt.variable.name]
            for dependency in stmt.dependencies:
                dest = self.variable_indices[dependency.name]
                self.dependency_graph[source].append(dest)

        for dest, sources in self.dependency_graph.items():
            for source in sources:
                self.dataflow_graph[source].append(dest)

    def variables(self):
        for stmt in self.statements:
            yield stmt.variable.name

    def dataflow(self, stmt):
        index = self.variable_indices[stmt.variable.name]
        for destination in self.dataflow_graph[index]:
            yield self.statements[destination]

    def __str__(self):
        return "\n".join(str(stmt) for stmt in self.statements)

    @classmethod
    def of_json(cls, json):
        statements = [statement.of_json(s) for s in json]
        return cls(statements)

    def topological_statements(self):
        # instead of removing edges, we just track which variables have been resolved
        resolved_variables = set()
        def is_resolved(stmt):
            return all(v.name in resolved_variables for v in stmt.dependencies)
        # collect all statements with no parents in a list
        initial_statements = [s for s in self.statements if is_resolved(s)]
        # iteratively mark edges from initial statements as "resolved"
        while initial_statements:
            # if it's in the initial list, we've already handled the deps
            stmt = initial_statements.pop()
            yield stmt
            # extend initial with any dataflow children that are now resolved
            resolved_variables.add(stmt.variable.name)
            initial_statements += [s for s in self.dataflow(stmt) if is_resolved(s)]

    def to_pyro(self, parameter_mapping):
        variable_mapping = {}
        for statement in self.topological_statements():
            v, dist = statement.to_pyro(variable_mapping, parameter_mapping)
            variable_mapping[v.name] = dist
        return variable_mapping