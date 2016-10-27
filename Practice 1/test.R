GENE_IDs = levels(reduced_data$ID)
testframe = data.frame()

for(i in 1:length(GENE_IDs))
{
  sig_frame = cbind(GENE_IDs[i] , log2(mean(2^reduced_data[ reduced_data$ID==GENE_IDs[i] ,]$logCy5_Cy3))  )
  testframe = rbind(testframe, sig_frame)
}