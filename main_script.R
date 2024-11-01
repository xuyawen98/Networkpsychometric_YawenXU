###################Setp1. Load the Required Libraries and Data (load_data.R)


# Load the Required Libraries
library(bootnet)
library(dplyr)
library(nodeIdentifyR)
library(devtools)
library(readxl)
library(NetworkComparisonTest)
library(qgraph)
library(ggplot2)
library(networktools)

# Install nodeIdentifyR from GitHub if not already installed
if (!requireNamespace("nodeIdentifyR", quietly = TRUE)) {
  devtools::install_github("JasperNaberman/nodeIdentifyR")
}
library(nodeIdentifyR)

# Read the Data
GS <- read_excel("Desktop/Critical Thinking Intervention AI News/Raw Data Supplement_1.xlsx")
mydata <- read_excel("Desktop/Critical Thinking Intervention AI News/Raw Data Supplement_3.xlsx")

###################Step2. Estimate the Network (estimate_network.R)



# Estimate the Network using the Ising Model
gs_fit <- estimateNetwork(GS, default = "IsingFit")

# Estimate the Network using the EBICglasso Algorithm
group <- rep(c('Neutral', 'Negative', 'Positive'), each = 5)
network2 <- estimateNetwork(mydata, default = "EBICglasso", tuning = 0.5, corMethod = "cor", corArgs = list(method = "spearman", use = "pairwise.complete.obs"))

###################Step3. Simulate Interventions and Calculate Sum Scores (simulate_interventions.R)


# Extract the Edge Weight Matrix and Threshold Vector
edgeWeightMatrix <- gs_fit$graph
thresholdVector <- gs_fit$intercepts

# Simulate Responses and Calculate Sum Scores
gs_IsingSamples <- simulateResponses(edgeWeightMatrix, thresholdVector, "aggravating", 2)
gs_sumIsingSamples <- calculateSumScores(gs_IsingSamples)

###################Step4. Compare Networks and Assess Impact of Interventions (compare_networks.R)


# Bridge Stability Analysis
caseDroppingBoot <- bootnet(network2, boots = 1000, type = "case", statistics = c("bridgeStrength", "bridgeExpectedInfluence"), communities = group)
corStability(caseDroppingBoot)

# Non-parametric Bootstrap Method
nonParametricBoot <- bootnet(network2, boots = 1000, type = "nonparametric", statistics = c("bridgeStrength", "bridgeExpectedInfluence"), communities = group)

###################Step5. Visualize Network Structures and Centrality Measures (visualize_network.R)


# Plot the Network
plot(gs_fit, 
     layout = "spring",
     groups = group, 
     label.cex = 1.5, 
     label.color = 'black', 
     negDashed = TRUE, 
     legend = FALSE,
     nodeNames = colnames(gs_fit), 
     legend.cex = 0.4, 
     palette = "colorblind",
     legend.mode = 'style4', 
     posCol = "#FF33C1",
     negCol = "blue")

# Save the Network Plot Image
dev.print(png, file.path(Sys.getenv("HOME"), "Desktop", "network11.png"), width = 8, height = 8, units = "in", res = 500)

# Visualize Centrality Measures
centralityPlot(network2, include = c("Strength", "Betweenness", "ExpectedInfluence"), orderBy = "ExpectedInfluence")