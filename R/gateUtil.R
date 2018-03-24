# Returns _id of population,using either of the parentPopulationId or parentPopulation params.
parsePopulationArgs = function(parentPopulationId, parentPopulation, experimentId) {
  if (!is.null(parentPopulationId) && !is.null(parentPopulation)) {
    stop("Please specify only one of 'parentPopulation' or 'parentPopulationId'.")
  }

  if (!is.null(parentPopulation)) { # attempt to lookup by name
    pops = getPopulations(experimentId, params = list(
      query = sprintf("eq(name, \"%s\")", parentPopulation)
    ))
    if (!is.data.frame(pops)) {
      stop(sprintf("Population with the name '%s' does not exist in the experiment.", parentPopulation))
    }
    if (nrow(pops) > 1) {
      stop(sprintf(paste0(
        "More than one population with the name '%s' exists in the experiment. ",
        "Cannot find unambiguous parent population."), parentPopulation))
    }
    parentPopulationId = pops$`_id`
  }

  return(parentPopulationId)
}

# Assigns fcsFileId to body, using either of the fcsFileId or fcsFile params.
parseFcsFileArgs = function(body, tailoredPerFile, fcsFileId, fcsFile, experimentId) {
  body = c(body, list(tailoredPerFile = jsonlite::unbox(tailoredPerFile)))

  if (!tailoredPerFile) return(body) # not tailored
  if (is.null(fcsFileId) && is.null(fcsFile)) return(body) # global tailored gate
  
  if (!is.null(fcsFileId) && !is.null(fcsFile)) {
    stop("Please specify only one of 'fcsFile' or 'fcsFileId'.")
  }

  if (!is.null(fcsFile)) { # attempt to lookup by name
    files = getFcsFiles(experimentId, params = list(
      query = sprintf("eq(filename, \"%s\")", fcsFile),
      fields = "+_id"
    ))
    if (!is.data.frame(files)) {
      stop(sprintf("FCS file with the name '%s' does not exist in the experiment.", fcsFile))
    }
    if (nrow(files) > 1) {
      stop(sprintf(paste0(
        "More than one FCS file with the name '%s' exists in the experiment. ",
        "Cannot find unambiguous file to create tailored gate."), fcsFile))
    }
    fcsFileId = files$`_id`
  }

  body = c(body, list(fcsFileId = jsonlite::unbox(fcsFileId)))

  return(body)
}

# Creates a population under the \code{parentPopulation} (population object) or
# population with the ID \code{parentPopulationId}.
autoCreatePopulation = function(experimentId, name, gid, parentPopulationId = NULL,
                                parentPopulation = NULL) {

  if (is.null(parentPopulation) && !is.null(parentPopulationId)) {
    parentPopulation = getPopulation(experimentId, parentPopulationId)
  }

  if (is.null(parentPopulation)) {
    # Parent is "Ungated"
    gates = list(`$and` = c(gid))
  } else {
    parentPopulationId = parentPopulation$`_id`
    gates = jsonlite::fromJSON(parentPopulation$gates)
    if ("$and" %in% names(gates)) {
      if (is.list(gates$`$and`) && length(gates$`$and`) == 0) { # empty, {$and: []}
        # jsonlite deserializes empty arrays to lists instead of vectors
        gates = list(`$and` = c(gid))
      } else { # not empty, e.g. {$and: [gidX]}
        gates$`$and` = c(gates$`$and`, gid)
      }
    } else { # complex, e.g. {$not: [gidX]}
      gates = list(`$and` = list(gates, jsonlite::unbox(gid)))
    }
  }

  createPopulation(experimentId, name, gates, gid, parentPopulationId)
}

# Assigns common properties to the body, then makes the request.
commonGateCreate = function(body, name, gid,
                            experimentId,
                            parentPopulationId, parentPopulation,
                            tailoredPerFile, fcsFileId, fcsFile,
                            createPopulation) {

  checkDefined(experimentId)

  parentPopulationId = parsePopulationArgs(parentPopulationId, parentPopulation,
    experimentId)

  body = c(body, list(
    parentPopulationId = jsonlite::unbox(parentPopulationId),
    name = jsonlite::unbox(name),
    gid = jsonlite::unbox(gid)
  ))

  body = parseFcsFileArgs(body, tailoredPerFile, fcsFileId, fcsFile, experimentId)

  body = jsonlite::toJSON(body, null = "null", digits = NA)
  path = paste("experiments", experimentId, "gates", sep = "/")
  gateResp = basePost(path, body, list())

  if (createPopulation) {
    populationResp = autoCreatePopulation(experimentId, name, gid, parentPopulationId)
    return(list(
      gate = gateResp,
      population = populationResp
    ))
  } else {
    return(list(gate = gateResp))
  }
}
