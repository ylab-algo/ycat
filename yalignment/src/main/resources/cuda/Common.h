#pragma once

__const__ short negInfinity = -10000;
__const__ short charSize = 256;

__device__ void scoresTracebackStep(
        short primaryItem,
        short substitutionItem,
        short verticalItem,
        short horizontalItem,
        char symbol1,
        char symbol2,
        int* i,
        int* j,
        int* matchesCount);

__device__ void initialNil(
        short* primary,
        short* substitution,
        short* vertical,
        short* horizontal,
        int idx);

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
        short* substitutionScores);
