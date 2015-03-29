# bring in the meta data for the OMICS platforms
meta.adult = read.table("/media/data/test/data/mrna_adult.tsv", header=TRUE, sep="\t")
meta.pedi  = read.table("/media/data/test/data/mrna_paed.tsv",  header=TRUE, stringsAsFactors=TRUE, sep="\t")
