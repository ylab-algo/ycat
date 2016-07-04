#include "GlobalAffineAlignment.h"
#include "LocalAffineAlignment.h"
#include "SemiglobalAffineAlignment.h"

__device__ void alignmentInternal(
        void (*calculateValuesFunc)(int,int,int,int,int,int,char,char,short*,short*,short*,short*,short*,int,int),
        void (*scoresFunc)(int,int,int,int,short*,short*,short*,short*,int,int,int,int,char*,char*,int,float*),
        int seqsCount,
        int refsCount,
        char* seqs,
        char* refs,
        short* seqSizes,
        int seqsMaxSize,
        short* refSizes,
        int refsMaxSize,
        short* primary,
        short* substitution,
        short* horizontal,
        short* vertical,
        short* substitutionScores,
        short gapOpenPenalty,
        short gapExtendPenalty,
        float* similarityScores) {

    int seqIdx = blockIdx.x * blockDim.x + threadIdx.x;
    int refIdx = blockIdx.y * blockDim.y + threadIdx.y;

    if (seqIdx >= seqsCount)
        return;
    else {
        int seqMatrixSize = seqsMaxSize + 1;
        int refMatrixSize = refsMaxSize + 1;
        int matrixSize = seqMatrixSize * refMatrixSize;
        int seqOffset = seqIdx * refsCount * matrixSize;
        int refOffset = refIdx * matrixSize;
        int seqRefOffset = seqOffset + refOffset;

        for (int i = 0; i < seqSizes[seqIdx] + 1; i++) {
            for (int j = 0; j < refSizes[refIdx] + 1; j++) {
                int currentIdx = seqRefOffset + i * refMatrixSize + j;
                int previousHorIdx = currentIdx - 1;
                int previousVerIdx = seqRefOffset + (i - 1) * refMatrixSize + j;
                int previousDiagIdx = previousVerIdx - 1;

                calculateValuesFunc(
                    i,
                    j,
                    currentIdx,
                    previousHorIdx,
                    previousVerIdx,
                    previousDiagIdx,
                    refs[refIdx * refsMaxSize + j - 1],
                    seqs[seqIdx * seqsMaxSize + i - 1],
                    primary,
                    substitution,
                    horizontal,
                    vertical,
                    substitutionScores,
                    gapOpenPenalty,
                    gapExtendPenalty);
            }
        }

        scoresFunc(
            seqSizes[seqIdx],
            refSizes[refIdx],
            seqRefOffset,
            refMatrixSize,
            primary,
            substitution,
            horizontal,
            vertical,
            seqIdx,
            refIdx,
            seqsMaxSize,
            refsMaxSize,
            seqs,
            refs,
            refsCount,
            similarityScores);
    }
}

extern "C"
__global__ void alignment(
        char algorithmType,
        int seqsCount,
        int refsCount,
        char* seqs,
        char* refs,
        short* seqSizes,
        int seqsMaxSize,
        short* refSizes,
        int refsMaxSize,
        short* primary,
        short* substitution,
        short* horizontal,
        short* vertical,
        short* substitutionScores,
        short gapOpenPenalty,
        short gapExtendPenalty,
        float* similarityScores) {

    void (*calculateValuesFunc)(int,int,int,int,int,int,char,char,short*,short*,short*,short*,short*,int,int);
    void (*scoresFunc)(int,int,int,int,short*,short*,short*,short*,int,int,int,int,char*,char*,int,float*);
    if (algorithmType == 'l') {
        calculateValuesFunc = &calculateLocalValues;
        scoresFunc = &localSimilarityScores;
    }
    else if (algorithmType == 'g') {
        calculateValuesFunc = &calculateGlobalValues;
        scoresFunc = &globalSimilarityScores;
    }
    else {
        calculateValuesFunc = &calculateSemiglobalValues;
        scoresFunc = &semiglobalSimilarityScores;
    }

    alignmentInternal(
        calculateValuesFunc,
        scoresFunc,
        seqsCount,
        refsCount,
        seqs,
        refs,
        seqSizes,
        seqsMaxSize,
        refSizes,
        refsMaxSize,
        primary,
        substitution,
        horizontal,
        vertical,
        substitutionScores,
        gapOpenPenalty,
        gapExtendPenalty,
        similarityScores);
}
