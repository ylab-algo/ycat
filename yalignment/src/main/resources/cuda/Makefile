semiglobalQualityAlignment:
	nvcc -cubin -arch=sm_$(arch) -dlink $(baseDir)/SemiglobalQualityAlignment.cu \
	$(baseDir)/Common.cu -o SemiglobalQualityAlignment.cubin
affineAlignment:
	nvcc -cubin -arch=sm_$(arch) -dlink $(baseDir)/AffineAlignment.cu $(baseDir)/LocalAffineAlignment.cu \
	$(baseDir)/GlobalAffineAlignment.cu $(baseDir)/SemiglobalAffineAlignment.cu \
	$(baseDir)/Common.cu -o AffineAlignment.cubin
