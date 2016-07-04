#include "Common.h"

__device__ void scoresTracebackStep(
        short primaryItem,
        short substitutionItem,
        short verticalItem,
        short horizontalItem,
        char symbol1,
        char symbol2,
        int* i,
        int* j,
        int* matchesCount) {

    if (primaryItem == substitutionItem) {
        if (symbol1 == symbol2) *matchesCount += 1;
        *i -= 1;
        *j -= 1;
    }
    else if (primaryItem == verticalItem)
        *i -= 1;
    else if (primaryItem == horizontalItem)
        *j -= 1;
}

__device__ void initialNil(
        short* primary,
        short* substitution,
        short* vertical,
        short* horizontal,
        int idx) {

    horizontal[idx] = vertical[idx] = substitution[idx] = primary[idx] = 0;
}

__device__ void setMatricesValues(
        short* primary,
        short* substitution,
        short* vertical,
        short* horizontal,
        int currentIdx,
        int previousHorIdx,
        int previousVerIdx,
        int previousDiagIdx,
        int gapExtendPenalty,
        int gapOpenPenalty,
        char horizontalItem,
        char verticalItem,
        short* substitutionScores) {

    short horItem = horizontal[currentIdx] = max(
        horizontal[previousHorIdx] + gapExtendPenalty,
        primary[previousHorIdx] + gapOpenPenalty
    );
    short verItem = vertical[currentIdx] = max(
        vertical[previousVerIdx] + gapExtendPenalty,
        primary[previousVerIdx] + gapOpenPenalty
    );
    short subsItem = substitution[currentIdx] = primary[previousDiagIdx] +
        substitutionScores[horizontalItem * charSize + verticalItem];

    primary[currentIdx] = max(subsItem, max(verItem, horItem));
}
