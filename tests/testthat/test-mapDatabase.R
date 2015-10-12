graph <- startGraph("http://localhost:7474/db/data", username = "neo4j",
  password = "stayagile")

if (FALSE) {
  clear(graph)

  ## Constraints //
  addConstraint(graph, "Namespace", "id")
  addConstraint(graph, "Timeseries", "id")
  addConstraint(graph, "Project", "id")
  addConstraint(graph, "Variant", "id")
  addConstraint(graph, "Company", "id")

  ## Nodes //
  createDb <- function(x) {
    ns <- if (!is.null(id <- x$ns_id)) {
      getOrCreateNode(graph, "Namespace", id = id)
    }
    ts <- if (!is.null(id <- x$ts_id)) {
      getOrCreateNode(graph, "Timeseries", id = id)
    }
    project <- if (!is.null(id <- x$project_id)) {
      getOrCreateNode(graph, "Project", id = id)
    }
    variant <- if (!is.null(id <- x$variant_id)) {
      getOrCreateNode(graph, "Variant", id = id)
    }
    company <- if (!is.null(id <- x$company_id)) {
      getOrCreateNode(graph, "Company", id = id)
    }

    #       rels <- getRels(graph, "MATCH (:Namespace {id:{id_ns}})-[h:HAS_TS]->(:Timeseries {id:{id_ts}}) RETURN h",
    #         id_ns = "ns_1", id_ts = "a")
    #       rels <- getRels(graph, "MATCH (:Namespace {id:{id_ns}})-[h:HAS_TS]->(:Timeseries {id:{id_ts}}) RETURN h",
    #         id_ns = ns, id_ts = ts)

    if (!is.null(ns) && !is.null(ts)) {
      query <- sprintf("
        MATCH (ns:Namespace)-[h:HAS_TS]->(ts:Timeseries)
        WHERE ns.id = '%s' AND ts.id = '%s'
        RETURN count(h) AS count
        ", x$ns_id, x$ts_id)
      count <- unlist(cypher(graph, query))

      if(count == 0) {
        createRel(ns, "HAS_TS", ts)
      }
    }

    if (!is.null(company) && !is.null(ts)) {
      query <- sprintf("
        MATCH (co:Company)-[h:HAS_TS]->(ts:Timeseries)
        WHERE co.id = '%s' AND ts.id = '%s'
        RETURN count(h) AS count
        ", x$company_id, x$ts_id)
      count <- unlist(cypher(graph, query))

      if(count == 0) {
        createRel(company, "HAS_TS", ts)
      }
    }

    if (!is.null(ts) && !is.null(project)) {
      query <- sprintf("
        MATCH (ts:Timeseries)-[h:HAS_PROJECT]->(project:Project)
        WHERE ts.id = '%s' AND project.id = '%s'
        RETURN count(h) AS count
        ", x$ts_id, x$project_id)
      count <- unlist(cypher(graph, query))

      if(count == 0) {
        createRel(ts, "HAS_PROJECT", project)
      }
    }

    if (!is.null(project) && !is.null(variant)) {
      query <- sprintf("
        MATCH (project:Project)-[h:HAS_VARIANT]->(variant:Variant)
        WHERE project.id = '%s' AND variant.id = '%s'
        RETURN count(h) AS count
        ", x$project_id, x$variant_id)
      count <- unlist(cypher(graph, query))

      if(count == 0) {
        createRel(project, "HAS_VARIANT", variant)
      }
    }

    TRUE
  }

  createDb(x = list(ns_id = "ns_1", ts_id = "a", project_id = "a_v1.0",
    variant_id = "a_v1.0.1"))
  createDb(list(ns_id = "ns_1", ts_id = "a", project_id = "a_v1.0",
    variant_id = "a_v1.0.2"))
  createDb(list(ns_id = "ns_1", ts_id = "a", project_id = "a_v1.0",
    variant_id = "a_v1.0.3"))

  createDb(list(ns_id = "ns_1", ts_id = "a", project_id = "a_v1.1",
    variant_id = "a_v1.1.1"))
  createDb(list(ns_id = "ns_1", ts_id = "a", project_id = "a_v1.1",
    variant_id = "a_v1.1.2"))
  createDb(list(ns_id = "ns_1", ts_id = "a", project_id = "a_v1.1",
    variant_id = "a_v1.1.3"))

  createDb(list(ns_id = "ns_1", ts_id = "b", project_id = "b_v1.0",
    variant_id = "b_v1.0.1"))
  createDb(list(ns_id = "ns_1", ts_id = "b", project_id = "b_v1.0",
    variant_id = "b_v1.0.2"))
  createDb(list(ns_id = "ns_1", ts_id = "b", project_id = "b_v1.0",
    variant_id = "b_v1.0.3"))

  createDb(list(ns_id = "ns_1", ts_id = "b", project_id = "b_v1.1",
    variant_id = "b_v1.1.1"))
  createDb(list(ns_id = "ns_1", ts_id = "b", project_id = "b_v1.1",
    variant_id = "b_v1.1.2"))
  createDb(list(ns_id = "ns_1", ts_id = "b", project_id = "b_v1.1",
    variant_id = "b_v1.1.3"))

  createDb(x = list(company_id = "company_1", ts_id = "a"))
  createDb(list(company_id = "company_1", ts_id = "b"))

  createDb(x = list(company_id = "company_2", ts_id = "a"))
  createDb(list(company_id = "company_2", ts_id = "b"))
}

# mapDatabase_1 ----------------------------------------------------------

context("mapDatabase_1")

test_that("mapDatabase::default", {
  expect_is(res <- mapDatabase(graph), "list")
  expect_true(length(res) > 0)
  expect_identical(names(res), c("ns_1", "company_1", "company_2"))
})
