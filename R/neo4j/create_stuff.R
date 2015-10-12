# db_path <- file.path(Sys.getenv("HOME"), "Neo4j/crud")
graph = startGraph("http://localhost:7474/db/data/",
  username = "neo4j", password = "stayagile")

# importSample(graph, "dfw")
# summary(graph)

clear(graph)

addConstraint(graph, "Host", "id")
addConstraint(graph, "Host", "name")

addConstraint(graph, "Query", "id")
addConstraint(graph, "Query", "name")

addConstraint(graph, "Host", "id")
