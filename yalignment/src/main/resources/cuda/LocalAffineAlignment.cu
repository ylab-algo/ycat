#include "Common.h"

__device__ void calculateLocalValues(
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
        initialNil(primary, substitution, vertical, horizontal, currentIdx);

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
    primary[currentIdx] = max(0, primary[currentIdx]);
}

__device__ void localSimilarityScores(
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

    int currentIdx, i, j, matchesCount = 0;
    short max = negInfinity, maxI, maxJ;

    for (i = 0; i < seqSize + 1; i++) {
        for (j = 0; j < refSize + 1; j++) {
            currentIdx = seqRefOffset + i * refMatrixSize + j;
            if (primary[currentIdx] > max) {
                max = primary[currentIdx];
                maxI = i;
                maxJ = j;
            }
        }
    }

    i = maxI;
    j = maxJ;
    currentIdx = seqRefOffset + i * refMatrixSize + j;

    while (i != 0 && j != 0 && primary[currentIdx] != 0) {
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

        currentIdx = seqRefOffset + i * refMatrixSize + j;
    }

    similarityScores[seqIdx * refsCount + refIdx] = (float)matchesCount / seqSize;
}
