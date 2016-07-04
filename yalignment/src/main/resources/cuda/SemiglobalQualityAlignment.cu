#include "Common.h"

__device__ char* setChar(char *dest, const char ch, int offset, int position) {
    dest[offset + position] = ch;
    return dest;
}

__device__ void calculateSemiglobalQualityAlignmentValues(
        int i,
        int j,
        int k,
        char horizontalItem,
        char verticalItem,
        short* primary,
        short* substitution,
        short* horizontal,
        short* vertical,
        short* substitutionScores,
        int size,
        int gapOpenPenalty,
        int gapExtendPenalty) {

    int pairOffset = k * size * size;
    int currentIdx = pairOffset + i * size + j;

    if (i == 0 || j == 0) {
        initialNil(primary, substitution, vertical, horizontal, currentIdx);

        return;
    }

    int previousHorIdx = currentIdx - 1;
    int previousVerIdx = pairOffset + (i - 1) * size + j;
    int previousDiagIdx = previousVerIdx - 1;

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

__device__ char* consensusItem(char aQuality, char bQuality, char a, char b) {
    char result[2] = {
        (aQuality >= bQuality) ? a : b,
        max(aQuality, bQuality)
    };

    return result;
}

__device__ void consensuses(
        int k,
        char* a,
        char* b,
        int maxSize,
        short fRealSize,
        short rRealSize,
        short* primary,
        short* substitution,
        short* horizontal,
        short* vertical,
        char* aQualities,
        char* bQualities,
        char gapQuality,
        char* consensus,
        char* consensusQualities,
        short* scores) {

    int pairOffset = k * maxSize * maxSize;
    int readOffset = k * (maxSize -1);
    int consensusOffset = readOffset * 2;
    short n;

    short verMax = 0, verMaxIndex = 0, horMax = 0, horMaxIndex = 0;
    for (n = 0; n < rRealSize + 1; n++) {
        short currentVer = primary[pairOffset + n * maxSize + fRealSize];
        if (currentVer > verMax) {
            verMax = currentVer;
            verMaxIndex = n;
        }
    }
    for (n = 0; n < fRealSize + 1; n++) {
        short currentHor = primary[pairOffset + rRealSize * maxSize + n];
        if (currentHor > horMax) {
            horMax = currentHor;
            horMaxIndex = n;
        }
    }

    int i = rRealSize, j = fRealSize;
    short mCon = 0, mQual = 0;

    //traceback end
    if (horMax >= verMax) { //deletions
        for (n = 0; n < fRealSize - horMaxIndex; n++) {
            char aQ = aQualities[readOffset + j - n - 1];
            if (aQ >= gapQuality) {
                setChar(consensus, a[readOffset + j - n - 1], consensusOffset, mCon++);
                setChar(consensusQualities, aQ, consensusOffset, mQual++);
            }
        }
        j = horMaxIndex;
    }
    else { //insertions
        for (n = 0; n < rRealSize - verMaxIndex; n++) {
            char bQ = bQualities[readOffset + i - n - 1];
            if (bQ > gapQuality) {
                setChar(consensus, b[readOffset + i - n - 1], consensusOffset, mCon++);
                setChar(consensusQualities, bQ, consensusOffset, mQual++);
            }
        }
        i = verMaxIndex;
    }

    //traceback middle
    while (i > 0 && j > 0) {
        short primaryItem = primary[pairOffset + i * maxSize + j];

        if (primaryItem == substitution[pairOffset + i * maxSize + j]) { //match or mismatch
            char* _consensusItem = consensusItem(
                                    aQualities[readOffset + j - 1],
                                    bQualities[readOffset + i - 1],
                                    a[readOffset + j - 1],
                                    b[readOffset + i - 1]);
            setChar(consensus, _consensusItem[0], consensusOffset, mCon++);
            setChar(consensusQualities, _consensusItem[1], consensusOffset, mQual++);

            i -= 1;
            j -= 1;
        }
        else if (primaryItem == vertical[pairOffset + i * maxSize + j]) { //insertion
            char bQ = bQualities[readOffset + i - 1];
            if (bQ > gapQuality) {
                setChar(consensus, b[readOffset + i - 1], consensusOffset, mCon++);
                setChar(consensusQualities, bQ, consensusOffset, mQual++);
            }

            i -= 1;
        }
        else if (primaryItem == horizontal[pairOffset + i * maxSize + j]) { //deletion
            char aQ = aQualities[readOffset + j - 1];
            if (aQ >= gapQuality) {
                setChar(consensus, a[readOffset + j - 1], consensusOffset, mCon++);
                setChar(consensusQualities, aQ, consensusOffset, mQual++);
            }

            j -= 1;
        }
    }

    //traceback start
    if (i == 0) { //deletions
        for (n = 0; n < j; n++) {
            char aQ = aQualities[readOffset + j - n - 1];
            if (aQ >= gapQuality) {
                setChar(consensus, a[readOffset + j - n - 1], consensusOffset, mCon++);
                setChar(consensusQualities, aQ, consensusOffset, mQual++);
            }
        }
    }
    else if (j == 0) { //insertion
        for (n = 0; n < i; n++) {
            char bQ = bQualities[readOffset + i - n - 1];
            if (bQ > gapQuality) {
                setChar(consensus, b[readOffset + i - n - 1], consensusOffset, mCon++);
                setChar(consensusQualities, bQ, consensusOffset, mQual++);
            }
        }
    }

    for (n = mCon; n < (maxSize - 1) * 2; n++) {
        setChar(consensus, '*', consensusOffset, mCon++);
        setChar(consensusQualities, 0, consensusOffset, mQual++);
    }

    scores[k] = max(horMax, verMax);
}

extern "C"
__global__ void semiGlobalQualityAlignment(
        int pairsCount,
        char* a,
        char* b,
        short* primary,
        short* substitution,
        short* horizontal,
        short* vertical,
        short* substitutionScores,
        int maxSize,
        short* realSizes,
        int gapOpenPenalty,
        int gapExtendPenalty,
        char* aQualities,
        char* bQualities,
        char gapQuality,
        char* consensus,
        char* consensusQualities,
        short* scores) {

    int k = blockIdx.x * blockDim.x + threadIdx.x;
    if (k >= pairsCount)
        return;

    int readMaxSize = maxSize - 1;
    int fRealSize = realSizes[k * 2];
    int rRealSize = realSizes[k * 2 + 1];
    for (int i = 0; i < rRealSize + 1; i++) {
        for (int j = 0; j < fRealSize + 1; j++) {
            calculateSemiglobalQualityAlignmentValues(
                i,
                j,
                k,
                a[k * readMaxSize + j - 1],
                b[k * readMaxSize + i - 1],
                primary,
                substitution,
                horizontal,
                vertical,
                substitutionScores,
                maxSize,
                gapOpenPenalty,
                gapExtendPenalty);
        }
    }

    consensuses(
        k,
        a,
        b,
        maxSize,
        fRealSize,
        rRealSize,
        primary,
        substitution,
        horizontal,
        vertical,
        aQualities,
        bQualities,
        gapQuality,
        consensus,
        consensusQualities,
        scores);
}
