#include "Common.h"

__device__ void calculateGlobalValues(
        int i,
        int j,
        int currentIdx,
        int previousHorIdx,
        int previousVerIdx,
        int previousDiagIdx,
        char horizontalItem,
        char verticalItem,
        short* primary,
        short* substitution,
        short* horizontal,
        short* vertical,
        short* substitutionScores,
        int gapOpenPenalty,
        int gapExtendPenalty) {

    if (i == 0 || j == 0) {
        if (i == 0) {
            horizontal[currentIdx] = gapOpenPenalty + j * gapExtendPenalty;
            vertical[currentIdx] = substitution[currentIdx] = primary[currentIdx] = negInfinity;
        }
        if (j == 0) {
            vertical[currentIdx] = gapOpenPenalty + i * gapExtendPenalty;
            if (i != 0) horizontal[currentIdx] = negInfinity;
            substitution[currentIdx] = primary[currentIdx] = negInfinity;
        }
        if (i == 0 && j == 0) primary[currentIdx] = 0;

        return;
    }

    setMatricesValues(
        primary,
        substitution,
        vertical,
        horizontal,
        currentIdx,
        previousHorIdx,
        previousVerIdx,
        previousDiagIdx,
        gapExtendPenalty,
        gapOpenPenalty,
        horizontalItem,
        verticalItem,
        substitutionScores);
}

__device__ void globalSimilarityScores(
        int seqSize,
        int refSize,
        int seqRefOffset,
        int refMatrixSize,
        short* primary,
        short* substitution,
        short* horizontal,
        short* vertical,
        int seqIdx,
        int refIdx,
        int seqsMaxSize,
        int refsMaxSize,
        char* seqs,
        char* refs,
        int refsCount,
        float* similarityScores) {

    int currentIdx, i = seqSize, j = refSize, matchesCount = 0;
    while (i != 0 && j != 0) {
        currentIdx = seqRefOffset + i * refMatrixSize + j;
        scoresTracebackStep(
            primary[currentIdx],
            substitution[currentIdx],
            vertical[currentIdx],
            horizontal[currentIdx],
            refs[refIdx * refsMaxSize + j - 1],
            seqs[seqIdx * seqsMaxSize + i - 1],
            &i,
            &j,
            &matchesCount);
    }

    similarityScores[seqIdx * refsCount + refIdx] = (float)matchesCount / seqSize;
}
