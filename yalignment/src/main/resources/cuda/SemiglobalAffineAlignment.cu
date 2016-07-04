#include "Common.h"

__device__ void calculateSemiglobalValues(
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
}

__device__ void semiglobalSimilarityScores(
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

    int i, j, tmpIdx;

    short maxIIdx = 0, maxJIdx = 0, maxI = negInfinity, maxJ = negInfinity;
    for (i = 0; i < seqSize + 1; i++) {
        tmpIdx = seqRefOffset + i * refMatrixSize + refSize;
        if (primary[tmpIdx] > maxI) {
            maxIIdx = i;
            maxI = primary[tmpIdx];
        }
    }
    for (j = 0; j < refSize + 1; j++) {
        tmpIdx = seqRefOffset + seqSize * refMatrixSize + j;
        if (primary[tmpIdx] > maxJ) {
            maxJIdx = j;
            maxJ = primary[tmpIdx];
        }
    }

    int matchesCount = 0;
    i = seqSize, j = refSize;
    if (maxJ >= maxI) //deletions
        j = maxJIdx;
    else  //insertions
        i = maxIIdx;

    while (i > 0 && j > 0) {
        int currentIdx = seqRefOffset + i * refMatrixSize + j;
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
