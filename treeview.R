library(data.tree)
library(jsonlite)

myColors <- c("#967b7b", # Compound
             "#e7e1c7", # Alternative
			 "#88a1a3", # Optional
			 "#87a384", # Selected
             "#f2a6b1") # Unselected

processDesign <- function(me, parent) {
  if(me$Type == "Component") {
    meNode <- parent$AddChild(me$Name)
	meNode$type <- me$Type
	meNode$selected <- me$Selected
	if(me$Selected) {
	  SetNodeStyle(meNode, inherit = FALSE,
	               fillcolor = myColors[4],
	               #fontcolor = "green",
				   penwidth = 2,
				   tooltip = "This node was selected.")
    } else {
	  SetNodeStyle(meNode, inherit = FALSE,
	               fillcolor = myColors[5],
	               fontcolor = "grey87",
				   tooltip = "This node was not selected.")
	}
  } else {
    meNode <- parent$AddChild(me$Name)
	meNode$type <- me$Type
	if (me$Type == "Optional") {
	  SetNodeStyle(meNode, inherit = FALSE,
	               fillcolor = myColors[3],
	               #fontcolor = "grey5",
				   tooltip = "This is an optional container.")
	} else if (me$Type == "Alternative") {
	  SetNodeStyle(meNode, inherit = FALSE,
	               fillcolor = myColors[2],
	               #fontcolor = "grey5",
				   tooltip = "This is an alternative container.")
	}
	for (i in 1:length(me$Children))
      processDesign(me$Children[[i]], meNode)
  }
}

IsVisible <- function(self) { (!is.null(self$selected) && self$selected) ||
                              (!is.null(self$children) && any(sapply(self$children, IsVisible))) }

consume <- function(tb) {
  tbm <- fromJSON(tb, simplifyDataFrame = FALSE)
  design <- tbm$Design
  #designTree <- as.Node(design)
  #print(designTree, "Name", "Type", "Selected")


  tree <- Node$new(design$Name)
  tree$type <- design$Type

  SetGraphStyle(tree, rankdir = "TB")
  SetEdgeStyle(tree, arrowhead = "vee", color = "grey35", penwidth = 2)
  SetNodeStyle(tree, style = "filled,rounded", shape = "box",
               fillcolor = myColors[1],
			   fontcolor = "#000000",
			   fontname = "helvetica",
			   tooltip = GetDefaultTooltip)
			 

  for (i in 1:length(design$Children)) {
    processDesign(design$Children[[i]], tree)
  }

  Sort(tree, "name")
  #tree$Set(visible = c(IsVisible))
  tree
}

tree <- consume("testbench_manifest_small.json")

print(tree, "name", "type", "selected")
Prune(tree, IsVisible)

# plot(tree)

print("Pruning Tree...............................................................")
prunedTree <- Clone(tree)
Prune(prunedTree, IsVisible)
print(prunedTree, "name", "type", "selected")
